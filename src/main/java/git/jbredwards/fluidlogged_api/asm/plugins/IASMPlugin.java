package git.jbredwards.fluidlogged_api.asm.plugins;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Iterator;
import java.util.Objects;

/**
 * used as a base for this mod's plugins
 * @author jbred
 *
 */
public interface IASMPlugin extends Opcodes
{
    int isMethodValid(@Nonnull MethodNode method, boolean obfuscated);
    boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index);
    //used to add local variables, returns the amount of variables added
    default int addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) { return 0; }
    //used to remove methods from classes (currently only used for betweenlands mod compat)
    default boolean removeMethod(@Nonnull Iterator<MethodNode> methods, boolean obfuscated, int index) { return false; }
    //ran when the handler transforms the class
    default byte[] transform(@Nonnull byte[] basicClass, boolean obfuscated) {
        final ClassNode classNode = new ClassNode();
        final ClassReader reader = new ClassReader(basicClass);
        reader.accept(classNode, 0);

        //runs through each method in the class to find the one that has to be transformed
        for(Iterator<MethodNode> it = classNode.methods.iterator(); it.hasNext();) {
            MethodNode method = it.next();
            int index = isMethodValid(method, obfuscated);
            if(index > 0) {
                //informs the console of the transformation
                informConsole(reader, method);

                //removes methods and skips the rest if so
                if(removeMethod(it, obfuscated, index)) continue;

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
                for(AbstractInsnNode insn : method.instructions.toArray()) {
                    //transforms the method
                    if(transform(method.instructions, method, insn, obfuscated, index)) break;
                }
            }
        }

        //writes the changes
        final ClassWriter writer = new ClassWriter(0);
        classNode.accept(writer);

        //returns the transformed class
        return writer.toByteArray();
    }

    //==============================================================
    //utility methods that are helpful when applying transformations
    //==============================================================

    default void informConsole(ClassReader reader, MethodNode method) {
        System.out.printf("Fluidlogged API Plugin: transforming... %s.%s%s%n", reader.getClassName(), method.name, method.desc);
    }

    @Nonnull
    default MethodInsnNode genMethodNode(@Nonnull String name, @Nonnull String desc) {
        return genMethodNode("git/jbredwards/fluidlogged_api/asm/plugins/ASMHooks", name, desc);
    }

    @Nonnull
    default MethodInsnNode genMethodNode(@Nonnull String clazz, @Nonnull String name, @Nonnull String desc) {
        return new MethodInsnNode(INVOKESTATIC, clazz, name, desc, false);
    }

    //same as the normal method, but this one can specify how many to go back
    @Nonnull
    default AbstractInsnNode getPrevious(@Nonnull AbstractInsnNode insn, int count) {
        for(int i = 0; i < count; i++) insn = Objects.requireNonNull(insn.getPrevious());
        return insn;
    }

    //same as the normal method, but this one can specify how many to go forward
    @Nonnull
    default AbstractInsnNode getNext(@Nonnull AbstractInsnNode insn, int count) {
        for(int i = 0; i < count; i++) insn = Objects.requireNonNull(insn.getNext());
        return insn;
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

    //same as above, but for method nodes
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
        else return((FieldInsnNode)insn).name.equals(name) && ((FieldInsnNode)insn).desc.equals(desc);
    }

    //sets the max locals while taking possible external transformers into account
    default void setMaxLocals(@Nonnull MethodNode method, int newMaxLocals) {
        if(method.maxLocals < newMaxLocals) method.maxLocals = newMaxLocals;
    }
}
