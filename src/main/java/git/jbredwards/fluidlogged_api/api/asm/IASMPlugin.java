/*
 * Copyright (c) 2024. jbredwards
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
import javax.xml.ws.Holder;
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
    @Nonnull Holder<String> ACTIVE_PLUGIN = new Holder<>("Unknown Plugin");
    static void resetActivePlugin() { ACTIVE_PLUGIN.value = "Unknown Plugin"; }
    static void setActivePlugin(@Nonnull final String plugin) { ACTIVE_PLUGIN.value = plugin; }

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
    default boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return false; }
    //transform a method, return true if the method is transformed
    default boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) { return true; }
    default boolean transform(@Nonnull final ClassNode classNode, @Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        return transform(instructions, method, insn, obfuscated, index);
    }
    //return false if the class has been transformed, returning false will cause method transforms to be skipped
    default boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) { return true; }
    //used to add local variables, returns true if variables were added
    default boolean addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) { return false; }
    //ran when the handler transforms the class
    default byte[] transform(@Nonnull byte[] basicClass, boolean obfuscated) {
        final ClassNode classNode = new ClassNode();
        new ClassReader(basicClass).accept(classNode, recalcFrames(obfuscated) ? ClassReader.SKIP_FRAMES : 0);
        if(transformClass(classNode, obfuscated)) {
            //runs through each method in the class to find the one that has to be transformed
            for(MethodNode method : classNode.methods) {
                int index = getMethodIndex(method, obfuscated);
                if(index != 0) {
                    //informs the console of the transformation
                    informConsole(classNode.name, method);
                    //used to help add any new local variables
                    LabelNode start = new LabelNode();
                    LabelNode end = new LabelNode();
                    //adds any new local variables
                    if(addLocalVariables(method, start, end, index)) {
                        //ensures that the new local variables can be called anywhere in the method
                        method.instructions.insertBefore(method.instructions.getFirst(), start);
                        method.instructions.insert(method.instructions.getLast(), end);
                    }
                    //runs through each node in the method
                    for(AbstractInsnNode insn : method.instructions.toArray())
                        //transforms the method
                        if(transform(classNode, method.instructions, method, insn, obfuscated, index)) break;
                }
            }
        }
        else informConsole(classNode.name, null);
        //writes the changes
        final ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_MAXS | (recalcFrames(obfuscated) ? ClassWriter.COMPUTE_FRAMES : 0));
        classNode.accept(writer);
        //returns the transformed class
        return writer.toByteArray();
    }

    /**
     * Can be useful for easily troubleshooting plugins.
     *
     * @param className The name of the class being transformed.
     * @param method The MethodNode being transformed, or null if the class itself is being transformed.
     * @throws NullPointerException If className is null.
     *
     * @since 1.9.0
     * @author jbred
     */
    default void informConsole(@Nonnull final String className, @Nullable final MethodNode method) {
        if(method == null) PLUGIN_LOGGER.debug(ACTIVE_PLUGIN.value + ": transforming... " + className);
        else PLUGIN_LOGGER.debug(ACTIVE_PLUGIN.value + ": transforming... " + className + '.' + method.name + method.desc);
    }

    //overrides all existing MethodNodes that match the search condition
    default void overrideMethod(@Nonnull ClassNode classNode, @Nonnull Predicate<MethodNode> searchCondition, @Nullable String hookName, @Nullable String hookDesc, @Nonnull Consumer<GeneratorAdapter> consumer) {
        for(@Nonnull final MethodNode method : classNode.methods) if(searchCondition.test(method)) overrideMethod(classNode, method, hookName, hookDesc, consumer);
    }

    //overrides an existing MethodNode
    default void overrideMethod(@Nonnull ClassNode classNode, @Nonnull MethodNode method, @Nullable String hookName, @Nullable String hookDesc, @Nonnull Consumer<GeneratorAdapter> consumer) {
        informConsole(classNode.name, method);
        //remove existing body data
        method.instructions.clear();
        if(method.tryCatchBlocks != null) method.tryCatchBlocks.clear();
        if(method.localVariables != null) method.localVariables.clear();
        if(method.visibleLocalVariableAnnotations != null) method.visibleLocalVariableAnnotations.clear();
        if(method.invisibleLocalVariableAnnotations != null) method.invisibleLocalVariableAnnotations.clear();
        //write new body data
        consumer.accept(new GeneratorAdapter(method, method.access, method.name, method.desc));
        if(hookName != null && hookDesc != null) //allow the hook to be skipped, in case it's easier to use the consumer
            method.visitMethodInsn(INVOKESTATIC, getHookClass(), hookName, hookDesc, false);
        method.visitInsn(Type.getReturnType(method.desc).getOpcode(IRETURN));
    }

    //same as method below, but doesn't use a signature
    default void addMethod(@Nonnull ClassNode classNode, @Nonnull String name, @Nonnull String desc, @Nullable String hookName, @Nullable String hookDesc, @Nonnull Consumer<GeneratorAdapter> consumer) {
        addMethod(classNode, name, desc, null, hookName, hookDesc, consumer);
    }

    //generates a new MethodNode
    default void addMethod(@Nonnull ClassNode classNode, @Nonnull String name, @Nonnull String desc, @Nullable String signature, @Nullable String hookName, @Nullable String hookDesc, @Nonnull Consumer<GeneratorAdapter> consumer) {
        if(classNode.methods.stream().filter(method -> method.name.equals(name) && method.desc.equals(desc)).peek(method -> overrideMethod(classNode, method, hookName, hookDesc, consumer)).count() != 0) return;
        // add new method if existing method was not found
        final MethodNode method = new MethodNode(ACC_PUBLIC, name, desc, signature, null);
        informConsole(classNode.name, method);
        //write new body data
        consumer.accept(new GeneratorAdapter(method, method.access, method.name, method.desc));
        if(hookName != null && hookDesc != null) //allow the hook to be skipped, in case it's easier to use the consumer
            method.visitMethodInsn(INVOKESTATIC, getHookClass(), hookName, hookDesc, false);
        method.visitInsn(Type.getReturnType(method.desc).getOpcode(IRETURN));
        //add the newly generated method
        classNode.methods.add(method);
    }

    /**
     * Removes all nodes from indexes 0 though n (inclusive), relative to the provided insn (representing index 0).
     *
     * @param instructions The list of instructions to modify.
     * @param insn The origin insn.
     * @param n How many nodes to remove after (can be negative to instead remove nodes before).
     * @throws NullPointerException If instructions or insn are null.
     *
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
     * @throws NullPointerException If insn is null.
     *
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
     * @throws NullPointerException If insn is null.
     *
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
     * @throws NullPointerException If name or desc are null.
     *
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
     * @throws NullPointerException If any parameters are null.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default MethodInsnNode genMethodNode(@Nonnull final String owner, @Nonnull final String name, @Nonnull final String desc) {
        return new MethodInsnNode(INVOKESTATIC, owner, name, desc, false);
    }

    @Nonnull
    default LocalVariableNode findLocal(@Nonnull final MethodNode method, @Nonnull final String name, @Nonnull final String desc) {
        return (method.localVariables == null ? Collections.<LocalVariableNode>emptyList() : method.localVariables).stream()
                .filter(var -> var.name.equals(name) && var.desc.equals(desc))
                .findFirst().orElseThrow(() -> new TypeNotPresentException(String.format(
                        "Could not find local variable: {name: \"%s\", desc: \"%s\"} in method: {name: \"%s\", desc: \"%s\"}",
                        name, desc, method.name, method.desc), null));
    }

    //same as below, but for method nodes
    default boolean checkMethod(@Nonnull final MethodNode method, @Nullable final String name, @Nullable final String desc) {
        //if both are null, assume looking for any method
        if(name == null && desc == null) return true;
        //if name null, assume only looking for desc
        else if(name == null) return method.desc.equals(desc);
        //if desc null, assume only looking for name
        else if(desc == null) return method.name.equals(name);
        //default return
        else return method.name.equals(name) && method.desc.equals(desc);
    }

    //returns true if the insn is both a method and if it matches the name & desc
    default boolean checkMethod(@Nullable final AbstractInsnNode insn, @Nullable final String name, @Nullable final String desc) {
        //dude it isn't even a method...
        if(!(insn instanceof MethodInsnNode)) return false;
        //if both are null, assume looking for any method
        else if(name == null && desc == null) return true;
        //if name null, assume only looking for desc
        else if(name == null) return ((MethodInsnNode)insn).desc.equals(desc);
        //if desc null, assume only looking for name
        else if(desc == null) return ((MethodInsnNode)insn).name.equals(name);
        //default return
        else return ((MethodInsnNode)insn).name.equals(name) && ((MethodInsnNode)insn).desc.equals(desc);
    }

    //utility method that doesn't take in a desc
    default boolean checkMethod(@Nullable final AbstractInsnNode insn, @Nonnull final String name) {
        return insn instanceof MethodInsnNode && ((MethodInsnNode)insn).name.equals(name);
    }

    //returns true if the insn is both a field and if it matches the name & desc
    default boolean checkField(@Nullable final AbstractInsnNode insn, @Nullable final String name, @Nullable final String desc) {
        //not a field
        if(!(insn instanceof FieldInsnNode)) return false;
        //if all are null, assume looking for any field
        else if(name == null && desc == null) return true;
        //only looking for desc
        else if(name == null) return ((FieldInsnNode)insn).desc.equals(desc);
        //only looking for name
        else if(desc == null) return ((FieldInsnNode)insn).name.equals(name);
        //default
        else return ((FieldInsnNode)insn).name.equals(name) && ((FieldInsnNode)insn).desc.equals(desc);
    }

    //utility method that doesn't take in a desc
    default boolean checkField(@Nullable final AbstractInsnNode insn, @Nonnull final String name) {
        return insn instanceof FieldInsnNode && ((FieldInsnNode)insn).name.equals(name);
    }

    //disable recalc frames by default since some classes don't like it (mainly obfuscated vanilla ones)
    //that being said, the option exists to enable them for transformers that need it
    default boolean recalcFrames(final boolean obfuscated) { return false; }

    /**
     * @return The class name of a nested hook class, used by
     * {@link IASMPlugin#genMethodNode(String, String) IASMPlugin.genMethodNode}.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default String getHookClass() { return getClass().getName().replace('.', '/') + "$Hooks"; }

    /**
     * @return The name of a nested accessor interface.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    default String getAccessorClass() { return getClass().getName().replace('.', '/') + "$Accessor"; }

    /**
     * @param format A format method descriptor string.
     * @return A formatted method descriptor string, with the accessor class added.
     * @throws NullPointerException If format is null.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    default String withAccessorClass(@Nonnull final String format) { return format.replace("%s", getAccessorClass()); }
}
