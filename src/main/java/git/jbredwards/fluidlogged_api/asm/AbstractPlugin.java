package git.jbredwards.fluidlogged_api.asm;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;
import java.util.Iterator;
import java.util.List;

/**
 * PLUGIN METHODS & FIELDS SHOULD ONLY BE ACCESS THROUGH THEIR OWN MOD
 * used as a base class for plugins
 * @author jbred
 *
 */
public abstract class AbstractPlugin implements Opcodes
{
    //returns the method being transformed
    @Nullable public abstract String getMethodName(boolean obfuscated);
    //returns the desc of the method being transformed
    @Nullable public abstract String getMethodDesc();
    //ran once for each node in the method, return true if the transformation is finished
    public abstract boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated);
    //used to add local variables, returns the amount of variables added
    public int addLocalVariables(List<LocalVariableNode> variables, LabelNode start, LabelNode end) { return 0; }
    //used to remove methods from classes (currently only used for betweenlands modded)
    public boolean removeMethod(Iterator<MethodNode> methods, boolean obfuscated) { return false; }

    //checks if the method is the one that has to be transformed
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        return ASMUtils.checkMethod(method, getMethodName(obfuscated), getMethodDesc());
    }

    //I got tired of typing mostly the same stuff for method insn nodes
    public MethodInsnNode method(String name, String desc) {
        return new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", name, desc, false);
    }

    //ran when the handler transforms the class
    public byte[] transform(byte[] basicClass, boolean obfuscated) {
        final ClassNode classNode = new ClassNode();
        final ClassReader reader = new ClassReader(basicClass);
        reader.accept(classNode, 0);

        //runs through each method in the class to find the one that has to be transformed
        all:for(Iterator<MethodNode> it = classNode.methods.iterator(); it.hasNext();) {
            MethodNode method = it.next();
            if(isMethodValid(method, obfuscated)) {
                //removes methods and skips the rest if so
                if(removeMethod(it, obfuscated)) continue;

                //used to help add any new local variables
                LabelNode start = new LabelNode();
                LabelNode end = new LabelNode();
                int localVariablesAdded = addLocalVariables(method.localVariables, start, end);

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
                    if(transform(method.instructions, method, insn, obfuscated)) break all;
                }
            }
        }

        final ClassWriter writer = new ClassWriter(0);
        classNode.accept(writer);

        //returns the transformed class
        return writer.toByteArray();
    }
}
