package git.jbredwards.fluidlogged_api.asm;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nullable;
import java.util.Iterator;

/**
 * used for plugins that transform multiple methods
 * @author jbred
 *
 */
public abstract class AbstractMultiMethodPlugin extends AbstractPlugin
{
    //used to cache the current method
    public int currentMethod = 0;
    //true if all transforms are done
    public boolean finishedAll = false;

    @Override
    public abstract boolean isMethodValid(MethodNode method, boolean obfuscated);

    //unused
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) {
        return null;
    }

    //unused
    @Nullable
    @Override
    public String getMethodDesc() {
        return null;
    }

    //copied from parent
    @Override
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
                    if(transform(method.instructions, method, insn, obfuscated)) {
                        currentMethod = 0;

                        //breaks all transforms if all are done
                        if(finishedAll) break all;
                        else break;
                    }
                }
            }
        }

        final ClassWriter writer = new ClassWriter(0);
        classNode.accept(writer);

        //returns the transformed class
        return writer.toByteArray();
    }
}
