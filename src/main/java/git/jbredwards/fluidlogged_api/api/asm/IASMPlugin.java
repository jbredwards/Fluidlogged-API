/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.asm;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;
import java.util.function.Consumer;
import java.util.function.Predicate;

/**
 * Offers utilities that allow for quick and easy asm-based class transformers.
 *
 * @since 1.9.0
 * @author jbred
 *
 */
public interface IASMPlugin extends Opcodes
{
    // set this to your mod's active transformer, this is to display the correct debug info in the console
    @Nonnull Logger PLUGIN_LOGGER = LogManager.getFormatterLogger();
    @Nonnull String[] ACTIVE_PLUGIN = new String[] {"Unknown Plugin"};
    static void resetActivePlugin() { ACTIVE_PLUGIN[0] = "Unknown Plugin"; }
    static void setActivePlugin(@Nonnull final String plugin) { ACTIVE_PLUGIN[0] = plugin; }

    /**
     * This method is run for each MethodNode in the ClassNode.
     *
     * @param method The MethodNode.
     * @param obfuscated True if this is being run from an obfuscated environment.
     * @return Index for the provided MethodNode, or 0 if it should be skipped. This value is passed into
     * {@link IASMPlugin#transform(InsnList, MethodNode, AbstractInsnNode, boolean, int) transform}.
     * @throws NullPointerException If method is null.
     *
     * @since 1.9.0
     * @author jbred
     */
    default int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        return isMethodValid(method, obfuscated) ? 1 : 0;
    }

    /**
     * This method is run for each MethodNode in the ClassNode.
     *
     * @param method The MethodNode.
     * @param obfuscated True if this is being run from an obfuscated environment.
     * @return True if the provided method should be transformed by
     * {@link IASMPlugin#transform(InsnList, MethodNode, AbstractInsnNode, boolean, int) transform}.
     * @throws NullPointerException If method is null.
     *
     * @since 1.9.0
     * @author jbred
     */
    default boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) {
        return false;
    }

    /**
     * Called by {@link IASMPlugin#transformNode} for each instruction in each method in the class.
     * @param instructions Method instructions being transformed.
     * @param method Method being transformed.
     * @param insn Currently selected node.
     * @param obfuscated True if this is being run from an obfuscated environment.
     * @param index Index of the method being transformed, supplied by {@link IASMPlugin#getMethodIndex}.
     * @return True if the MethodNode is fully transformed. This tells {@link IASMPlugin#transformNode} to stop iterating through the method's instructions, and sends a message to the debug logger.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.9.0
     * @author jbred
     */
    default boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        return true;
    }

    /**
     * Called by {@link IASMPlugin#transformNode} for each instruction in each method in the class.
     * @param classNode ClassNode being transformed.
     * @param instructions Method instructions being transformed.
     * @param method Method being transformed.
     * @param insn Currently selected node.
     * @param obfuscated True if this is being run from an obfuscated environment.
     * @param index Index of the method being transformed, supplied by {@link IASMPlugin#getMethodIndex}.
     * @return True if the MethodNode is fully transformed. This tells {@link IASMPlugin#transformNode} to stop iterating through the method's instructions, and sends a message to the debug logger.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 3.0.0
     * @author jbred
     */
    default boolean transform(@Nonnull final ClassNode classNode, @Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        return transform(instructions, method, insn, obfuscated, index);
    }

    /**
     * @param classNode ClassNode being transformed.
     * @param obfuscated True if this is being run from an obfuscated environment.
     * @return False if the class has been transformed, returning false will cause method transforms to be skipped.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.9.0
     * @author jbred
     */
    default boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        return true;
    }

    /**
     * Used to add local variables.
     * @param method Method being transformed.
     * @param start An instruction that's automatically inserted at the start of the method.
     * @param end An instruction that's automatically inserted at the end of the method.
     * @param index Index of the method being transformed, supplied by {@link IASMPlugin#getMethodIndex}.
     * @return True if variables were added.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.9.0
     * @author jbred
     */
    default boolean addLocalVariables(@Nonnull final MethodNode method, @Nonnull final LabelNode start, @Nonnull final LabelNode end, final int index) {
        return false;
    }

    /**
     * Responsible for transforming the class.
     * @param basicClass Old class bytecode.
     * @param obfuscated True if this is being run from an obfuscated environment.
     * @return The new bytecode for the class.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.9.0
     * @author jbred
     */
    default byte[] transform(@Nonnull final byte[] basicClass, final boolean obfuscated) {
        @Nonnull final ClassNode classNode = new ClassNode();
        new ClassReader(basicClass).accept(classNode, recalcFrames(obfuscated) ? ClassReader.SKIP_FRAMES : 0);
        transformNode(classNode, obfuscated);
        // writes the changes
        @Nonnull final ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_MAXS | (recalcFrames(obfuscated) ? ClassWriter.COMPUTE_FRAMES : 0));
        classNode.accept(writer);
        return writer.toByteArray();
    }

    /**
     * Responsible for transforming the ClassNode.
     * @param classNode ClassNode being transformed.
     * @param obfuscated True if this is being run from an obfuscated environment.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 3.0.0
     * @author jbred
     */
    default void transformNode(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        if(transformClass(classNode, obfuscated)) {
            // runs through each method in the class to find the one that has to be transformed
            for(@Nonnull final MethodNode method : classNode.methods) {
                int index = getMethodIndex(method, obfuscated);
                if(index != 0) {
                    // informs the console of the transformation
                    informConsole(classNode.name, method);
                    // used to help add any new local variables
                    @Nonnull final LabelNode start = new LabelNode();
                    @Nonnull final LabelNode end = new LabelNode();
                    // adds any new local variables
                    if(addLocalVariables(method, start, end, index)) {
                        // ensures that the new local variables can be called anywhere in the method
                        method.instructions.insertBefore(method.instructions.getFirst(), start);
                        method.instructions.insert(method.instructions.getLast(), end);
                    }
                    // runs through each node in the method
                    for(@Nonnull final AbstractInsnNode insn : method.instructions.toArray())
                        // transforms the method
                        if(transform(classNode, method.instructions, method, insn, obfuscated, index)) break;
                }
            }
        }
        else informConsole(classNode.name, null);
    }

    /**
     * Can be useful for easily troubleshooting plugins.
     *
     * @param className The name of the class being transformed.
     * @param method The MethodNode being transformed, or null if the class itself is being transformed.
     *
     * @throws NullPointerException If className is null.
     * @since 1.9.0
     * @author jbred
     */
    default void informConsole(@Nonnull final String className, @Nullable final MethodNode method) {
        if(shouldInformConsole()) {
            if(method == null) PLUGIN_LOGGER.debug(ACTIVE_PLUGIN[0] + ": transforming... " + className);
            else PLUGIN_LOGGER.debug(ACTIVE_PLUGIN[0] + ": transforming... " + className + '.' + method.name + method.desc);
        }
    }

    /**
     * Utility function that overrides all MethodNodes that match the search condition.
     * @param classNode ClassNode being transformed.
     * @param searchCondition MethodNode to override.
     * @param hookName Optional name for a method to call from the {@link IASMPlugin#getHookClass() Hook class}.
     * @param hookDesc Optional desc for a method to call from the {@link IASMPlugin#getHookClass() Hook class} (see {@link org.objectweb.asm.Type}).
     * @param consumer Generator for the new method instructions, automatically handles the return statement.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.9.0
     * @author jbred
     */
    default void overrideMethod(@Nonnull final ClassNode classNode, @Nonnull final Predicate<MethodNode> searchCondition, @Nullable final String hookName, @Nullable final String hookDesc, @Nonnull final Consumer<GeneratorAdapter> consumer) {
        for(@Nonnull final MethodNode method : classNode.methods) if(searchCondition.test(method)) overrideMethod(classNode, method, hookName, hookDesc, consumer);
    }

    /**
     * Utility function that overrides the provided MethodNode.
     * @param classNode ClassNode being transformed.
     * @param method MethodNode to override.
     * @param hookName Optional name for a method to call from the {@link IASMPlugin#getHookClass() Hook class}.
     * @param hookDesc Optional desc for a method to call from the {@link IASMPlugin#getHookClass() Hook class} (see {@link org.objectweb.asm.Type}).
     * @param consumer Generator for the new method instructions, automatically handles the return statement.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 3.0.0
     * @author jbred
     */
    default void overrideMethod(@Nonnull final ClassNode classNode, @Nonnull final MethodNode method, @Nullable final String hookName, @Nullable final String hookDesc, @Nonnull final Consumer<GeneratorAdapter> consumer) {
        informConsole(classNode.name, method);
        // remove existing body data
        method.instructions.clear();
        if(method.tryCatchBlocks != null) method.tryCatchBlocks.clear();
        if(method.localVariables != null) method.localVariables.clear();
        if(method.visibleLocalVariableAnnotations != null) method.visibleLocalVariableAnnotations.clear();
        if(method.invisibleLocalVariableAnnotations != null) method.invisibleLocalVariableAnnotations.clear();
        // write new body data
        consumer.accept(new GeneratorAdapter(method, method.access, method.name, method.desc));
        if(hookName != null && hookDesc != null) // allow the hook to be skipped, in case it's easier to use the consumer
            method.visitMethodInsn(INVOKESTATIC, getHookClass(), hookName, hookDesc, false);
        method.visitInsn(Type.getReturnType(method.desc).getOpcode(IRETURN));
    }

    /**
     * Utility function that adds a new method. If a method already exists with the same name and desc, it's overriden with this new method.
     * @param classNode ClassNode being transformed.
     * @param name Name of the method to add.
     * @param desc Desc of the method to add (see {@link org.objectweb.asm.Type}).
     * @param hookName Optional name for a method to call from the {@link IASMPlugin#getHookClass() Hook class}.
     * @param hookDesc Optional desc for a method to call from the {@link IASMPlugin#getHookClass() Hook class} (see {@link org.objectweb.asm.Type}).
     * @param consumer Generator for the new method instructions, automatically handles the return statement.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.9.0
     * @author jbred
     */
    default void addMethod(@Nonnull final ClassNode classNode, @Nonnull final String name, @Nonnull final String desc, @Nullable final String hookName, @Nullable final String hookDesc, @Nonnull final Consumer<GeneratorAdapter> consumer) {
        addMethod(classNode, name, desc, null, hookName, hookDesc, consumer);
    }

    /**
     * Utility function that adds a new method. If a method already exists with the same name and desc, it's overriden with this new method.
     * @param classNode ClassNode being transformed.
     * @param name Name of the method to add.
     * @param desc Desc of the method to add (see {@link org.objectweb.asm.Type}).
     * @param signature Signature of the method to add.
     * @param hookName Optional name for a method to call from the {@link IASMPlugin#getHookClass() Hook class}.
     * @param hookDesc Optional desc for a method to call from the {@link IASMPlugin#getHookClass() Hook class} (see {@link org.objectweb.asm.Type}).
     * @param consumer Generator for the new method instructions, automatically handles the return statement.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 3.0.0
     * @author jbred
     */
    default void addMethod(@Nonnull final ClassNode classNode, @Nonnull final String name, @Nonnull final String desc, @Nullable final String signature, @Nullable final String hookName, @Nullable final String hookDesc, @Nonnull final Consumer<GeneratorAdapter> consumer) {
        if(classNode.methods.stream().filter(method -> method.name.equals(name) && method.desc.equals(desc)).peek(method -> overrideMethod(classNode, method, hookName, hookDesc, consumer)).count() != 0) return;
        // add new method if existing method was not found
        final MethodNode method = new MethodNode(ACC_PUBLIC, name, desc, signature, null);
        informConsole(classNode.name, method);
        // write new body data
        consumer.accept(new GeneratorAdapter(method, method.access, method.name, method.desc));
        if(hookName != null && hookDesc != null) // allow the hook to be skipped, in case it's easier to use the consumer
            method.visitMethodInsn(INVOKESTATIC, getHookClass(), hookName, hookDesc, false);
        method.visitInsn(Type.getReturnType(method.desc).getOpcode(IRETURN));
        // add the newly generated method
        classNode.methods.add(method);
    }

    /**
     * Removes all nodes from indexes 0 though n (inclusive), relative to the provided insn (representing index 0).
     *
     * @param instructions The list of instructions to modify.
     * @param insn The origin insn.
     * @param n How many nodes to remove after (can be negative to instead remove nodes before).
     *          
     * @throws NullPointerException If instructions or insn are null.
     * @since 1.9.0
     * @author jbred
     */
    default void removeFrom(@Nonnull final InsnList instructions, @Nonnull final AbstractInsnNode insn, final int n) {
        if(n > 0) for(int i = 0; i < n; i++) instructions.remove(insn.getNext());
        else for(int i = 0; i > n; i--) instructions.remove(insn.getPrevious());
        instructions.remove(insn);
    }

    /**
     * @param insn The origin insn.
     * @param n How many instructions to go back.
     * @return The nth previous instruction in the list to which insn belongs. If the nth previous instruction
     * is null, this instead returns the closest nonnull instruction to n.
     * 
     * @throws NullPointerException If insn is null.
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default AbstractInsnNode getPrevious(@Nonnull final AbstractInsnNode insn, final int n) {
        @Nonnull AbstractInsnNode ret = insn;
        for(int i = 0; i < n && ret.getPrevious() != null; i++) ret = ret.getPrevious();
        return ret;
    }

    /**
     * @param insn The origin insn.
     * @param n How many instructions to go forward.
     * @return The nth next instruction in the list to which insn belongs. If the nth next instruction
     * is null, this instead returns the closest nonnull instruction to n.
     * 
     * @throws NullPointerException If insn is null.
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default AbstractInsnNode getNext(@Nonnull final AbstractInsnNode insn, final int n) {
        @Nonnull AbstractInsnNode ret = insn;
        for(int i = 0; i < n && ret.getNext() != null; i++) ret = ret.getNext();
        return ret;
    }

    /**
     * @param name Method name.
     * @param desc Method descriptor (see {@link org.objectweb.asm.Type}).
     * @return A new ({@link Opcodes#INVOKESTATIC INVOKESTATIC}) MethodInsnNode, owned by {@link IASMPlugin#getHookClass()}.
     * 
     * @throws NullPointerException If name or desc are null.
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default MethodInsnNode genMethodNode(@Nonnull final String name, @Nonnull final String desc) {
        return genMethodNode(getHookClass(), name, desc);
    }

    /**
     * @param owner The internal name of the method's owner class (see {@link org.objectweb.asm.Type#getInternalName() getInternalName}).
     * @param name Method name.
     * @param desc Method descriptor (see {@link org.objectweb.asm.Type}).
     * @return A new ({@link Opcodes#INVOKESTATIC INVOKESTATIC}) MethodInsnNode.
     * 
     * @throws NullPointerException If any parameters are null.
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default MethodInsnNode genMethodNode(@Nonnull final String owner, @Nonnull final String name, @Nonnull final String desc) {
        return new MethodInsnNode(INVOKESTATIC, owner, name, desc, false);
    }

    /**
     * @param method MethodNode to search.
     * @param name Name of the local variable.
     * @param desc Desc of the local variable (see {@link org.objectweb.asm.Type}).
     * @return First local variable that matches the provided name and desc in the MethodNode.
     *
     * @throws NullPointerException If any parameters are null.
     * @throws TypeNotPresentException If no such local variable was found.
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    default LocalVariableNode findLocal(@Nonnull final MethodNode method, @Nonnull final String name, @Nonnull final String desc) {
        return (method.localVariables == null ? Collections.<LocalVariableNode>emptyList() : method.localVariables).stream().filter(var -> var.name.equals(name) && var.desc.equals(desc)).findFirst()
                .orElseThrow(() -> new TypeNotPresentException(String.format("Could not find local variable: {name: \"%s\", desc: \"%s\"} in method: {name: \"%s\", desc: \"%s\"}", name, desc, method.name, method.desc), null));
    }

    /**
     * @param method MethodNode to check.
     * @param name Method name, null to ignore.
     * @param desc Method desc (see {@link org.objectweb.asm.Type}), null to ignore.
     * @return True if the MethodNode has the desired name and desc.
     *
     * @throws NullPointerException If method is null.
     * @since 1.9.0
     * @author jbred
     */
    default boolean checkMethod(@Nonnull final MethodNode method, @Nullable final String name, @Nullable final String desc) {
        // if both are null, assume looking for any method
        if(name == null && desc == null) return true;
        // if name null, assume only looking for desc
        else if(name == null) return method.desc.equals(desc);
        // if desc null, assume only looking for name
        else if(desc == null) return method.name.equals(name);
        // default return
        else return method.name.equals(name) && method.desc.equals(desc);
    }

    /**
     * @param insn Instruction to check.
     * @param name Method name, null to ignore.
     * @param desc Method desc (see {@link org.objectweb.asm.Type}), null to ignore.
     * @return True if the instruction is a MethodInsnNode with the desired name and desc.
     *
     * @since 1.9.0
     * @author jbred
     */
    default boolean checkMethod(@Nullable final AbstractInsnNode insn, @Nullable final String name, @Nullable final String desc) {
        // dude it isn't even a method...
        if(!(insn instanceof MethodInsnNode)) return false;
        // if both are null, assume looking for any method
        else if(name == null && desc == null) return true;
        // if name null, assume only looking for desc
        else if(name == null) return ((MethodInsnNode)insn).desc.equals(desc);
        // if desc null, assume only looking for name
        else if(desc == null) return ((MethodInsnNode)insn).name.equals(name);
        // default return
        else return ((MethodInsnNode)insn).name.equals(name) && ((MethodInsnNode)insn).desc.equals(desc);
    }

    /**
     * @param insn Instruction to check.
     * @param name Method name.
     * @return True if the instruction is a MethodInsnNode with the desired name.
     *
     * @since 1.9.0
     * @author jbred
     */
    default boolean checkMethod(@Nullable final AbstractInsnNode insn, @Nonnull final String name) {
        return insn instanceof MethodInsnNode && ((MethodInsnNode)insn).name.equals(name);
    }

    /**
     * @param insn Instruction to check.
     * @param name Field name, null to ignore.
     * @param desc Field desc (see {@link org.objectweb.asm.Type}), null to ignore.
     * @return True if the instruction is a FieldInsnNode with the desired name and desc.
     *
     * @since 1.9.0
     * @author jbred
     */
    default boolean checkField(@Nullable final AbstractInsnNode insn, @Nullable final String name, @Nullable final String desc) {
        // not a field
        if(!(insn instanceof FieldInsnNode)) return false;
        // if all are null, assume looking for any field
        else if(name == null && desc == null) return true;
        // only looking for desc
        else if(name == null) return ((FieldInsnNode)insn).desc.equals(desc);
        // only looking for name
        else if(desc == null) return ((FieldInsnNode)insn).name.equals(name);
        // default
        else return ((FieldInsnNode)insn).name.equals(name) && ((FieldInsnNode)insn).desc.equals(desc);
    }

    /**
     * @param insn Instruction to check.
     * @param name Field name.
     * @return True if the instruction is a FieldInsnNode with the desired name.
     *
     * @since 1.9.0
     * @author jbred
     */
    default boolean checkField(@Nullable final AbstractInsnNode insn, @Nonnull final String name) {
        return insn instanceof FieldInsnNode && ((FieldInsnNode)insn).name.equals(name);
    }

    /**
     * Disable recalc frames by default since some classes don't like it (mainly obfuscated vanilla ones).
     * That being said, the option exists for transformers that need it.
     * @param obfuscated True if this is being run from an obfuscated environment.
     * @return True if {@link IASMPlugin#transform(byte[], boolean) transform} should use the {@link ClassReader#SKIP_FRAMES SKIP_FRAMES} and {@link ClassWriter#COMPUTE_FRAMES COMPUTE_FRAMES} flags.
     *
     * @since 1.9.0
     * @author jbred
     */
    default boolean recalcFrames(final boolean obfuscated) {
        return false;
    }

    /**
     * @return Whether the transformer should inform the console of changes.
     *
     * @since 1.9.0
     * @author jbred
     */
    default boolean shouldInformConsole() {
        return true;
    }

    /**
     * @return The class name of a nested hook class, used by {@link IASMPlugin#genMethodNode(String, String) genMethodNode}.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default String getHookClass() {
        return getClass().getName().replace('.', '/') + "$Hooks";
    }

    /**
     * @return The class name of a nested accessor interface.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    default String getAccessorClass() {
        return getClass().getName().replace('.', '/') + "$Accessor";
    }

    /**
     * @param format A format method descriptor string.
     * @return A formatted method descriptor string, with the accessor class added.
     * @throws NullPointerException If format is null.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    default String withAccessorClass(@Nonnull final String format) {
        return format.replace("%s", getAccessorClass());
    }
}
