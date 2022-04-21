package git.jbredwards.fluidlogged_api.mod.asm.plugins;

import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * Used as a base for this mod's plugins
 * @author jbred
 *
 */
public interface IASMPlugin extends Opcodes
{
    //returns the method index, which is passed into this#transform, returning 0 will skip the method
    default int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) { return isMethodValid(method, obfuscated) ? 1 : 0; }
    //utility method that makes life easier if only one method is being transformed
    default boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return false; }
    //transform a method, return true if the method is transformed
    default boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) { return false; }
    //return false if the class has been transformed, returning false will cause method transforms to be skipped
    default boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) { return true; }
    //used to add local variables, returns the amount of variables added
    default int addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) { return 0; }
    //ran when the handler transforms the class
    default byte[] transform(@Nonnull byte[] basicClass, boolean obfuscated) {
        final ClassNode classNode = new ClassNode();
        new ClassReader(basicClass).accept(classNode, 0);
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
                    int localVariablesAdded = addLocalVariables(method, start, end, index);
                    //adds any new local variables
                    if(localVariablesAdded > 0) {
                        //ensures that the new local variables can be called anywhere in the method
                        method.instructions.insertBefore(method.instructions.getFirst(), start);
                        method.instructions.insert(method.instructions.getLast(), end);
                        //changes the max local variables to account for the new ones
                        method.maxLocals += localVariablesAdded;
                    }
                    //runs through each node in the method
                    for(AbstractInsnNode insn : method.instructions.toArray())
                        //transforms the method
                        if(transform(method.instructions, method, insn, obfuscated, index)) break;
                }
            }
        }
        else informConsole(classNode.name, null);
        //writes the changes
        final ClassWriter writer = new ClassWriter(0);
        classNode.accept(writer);
        //returns the transformed class
        return writer.toByteArray();
    }

    //can be useful for easily troubleshooting plugins
    default void informConsole(@Nonnull String className, @Nullable MethodNode method) {
        if(ConfigHandler.debugASMPlugins) {
            if(method == null) System.out.printf("Fluidlogged API Plugin: transforming... %s", className);
            else System.out.printf("Fluidlogged API Plugin: transforming... %s.%s%s%n", className, method.name, method.desc);
        }
    }

    //overrides an existing MethodNode
    default void overrideMethod(@Nonnull ClassNode classNode, @Nonnull Predicate<MethodNode> searchCondition, @Nullable String hookName, @Nullable String hookDesc, @Nonnull Consumer<GeneratorAdapter> consumer) {
        for(MethodNode method : classNode.methods) {
            if(searchCondition.test(method)) {
                //remove existing body data
                method.instructions.clear();
                if(method.tryCatchBlocks != null) method.tryCatchBlocks.clear();
                if(method.localVariables != null) method.localVariables.clear();
                if(method.visibleLocalVariableAnnotations != null) method.visibleLocalVariableAnnotations.clear();
                if(method.invisibleLocalVariableAnnotations != null) method.invisibleLocalVariableAnnotations.clear();
                //write new body data
                consumer.accept(new GeneratorAdapter(method, method.access, method.name, method.desc));
                if(hookName != null && hookDesc != null) //allow the hook to be skipped, in case it's easier to use the consumer
                    method.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/asm/plugins/ASMHooks", hookName, hookDesc, false);
                method.visitInsn(Type.getReturnType(method.desc).getOpcode(IRETURN));
            }
        }
    }

    //generates a new MethodNode
    default void addMethod(@Nonnull ClassNode classNode, @Nonnull String name, @Nonnull String desc, @Nullable String hookName, @Nullable String hookDesc, @Nonnull Consumer<GeneratorAdapter> consumer) {
        final MethodNode method = new MethodNode(ACC_PUBLIC, name, desc, null, null);
        //write new body data
        consumer.accept(new GeneratorAdapter(method, method.access, method.name, method.desc));
        if(hookName != null && hookDesc != null) //allow the hook to be skipped, in case it's easier to use the consumer
            method.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/asm/plugins/ASMHooks", hookName, hookDesc, false);
        method.visitInsn(Type.getReturnType(method.desc).getOpcode(IRETURN));
        //add the newly generated method
        classNode.methods.add(method);
    }

    //remove all nodes from indexes 0 though n
    default void removeFrom(@Nonnull InsnList instructions, @Nonnull AbstractInsnNode insn, int n) {
        final Supplier<AbstractInsnNode> toRemove = n < 0 ? insn::getPrevious : insn::getNext;
        if(n < 0) n = -n;
        for(int i = 0; i < n; i++) instructions.remove(toRemove.get());
        instructions.remove(insn);
    }

    //=============================================================================================================
    //utility methods that are helpful when applying transformations (prior to v1.7 these were found in ASMUtils)
    //=============================================================================================================

    @Nonnull
    default MethodInsnNode genMethodNode(@Nonnull String name, @Nonnull String desc) {
        return genMethodNode("git/jbredwards/fluidlogged_api/mod/asm/plugins/ASMHooks", name, desc);
    }

    @Nonnull
    default MethodInsnNode genMethodNode(@Nonnull String clazz, @Nonnull String name, @Nonnull String desc) {
        return new MethodInsnNode(INVOKESTATIC, clazz, name, desc, false);
    }

    //same as insn#getPrevious, but this one can specify how many to go back
    @Nonnull
    default AbstractInsnNode getPrevious(@Nonnull AbstractInsnNode insn, int count) {
        for(int i = 0; i < count; i++) insn = Optional.ofNullable(insn.getPrevious()).orElse(insn);
        return insn;
    }

    //same as insn#getNext, but this one can specify how many to go forward
    @Nonnull
    default AbstractInsnNode getNext(@Nonnull AbstractInsnNode insn, int count) {
        for(int i = 0; i < count; i++) insn = Optional.ofNullable(insn.getNext()).orElse(insn);
        return insn;
    }

    //same as below, but for method nodes
    default boolean checkMethod(@Nonnull MethodNode method, @Nullable String name, @Nullable String desc) {
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
    default boolean checkMethod(@Nullable AbstractInsnNode insn, @Nullable String name, @Nullable String desc) {
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
    default boolean checkMethod(@Nonnull AbstractInsnNode insn, @Nonnull String name) {
        return insn instanceof MethodInsnNode && ((MethodInsnNode)insn).name.equals(name);
    }

    //returns true if the insn is both a field and if it matches the name & desc
    default boolean checkField(@Nullable AbstractInsnNode insn, @Nullable String name, @Nullable String desc) {
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
    default boolean checkField(@Nonnull AbstractInsnNode insn, @Nonnull String name) {
        return insn instanceof FieldInsnNode && ((FieldInsnNode)insn).name.equals(name);
    }

    //sets the max locals while taking possible external transformers into account
    default void setMaxLocals(@Nonnull MethodNode method, int newMaxLocals) {
        if(method.maxLocals < newMaxLocals) method.maxLocals = newMaxLocals;
        //always update the max stack size to allow for an increased local size
        if(method.maxStack < newMaxLocals) method.maxStack = newMaxLocals;
    }
}
