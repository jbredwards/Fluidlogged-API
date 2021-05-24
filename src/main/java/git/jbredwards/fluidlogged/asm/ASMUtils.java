package git.jbredwards.fluidlogged.asm;

import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.MethodInsnNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 * some helpful functions for asm class transformers
 * @author jbred
 *
 */
public enum ASMUtils
{
    ;

    //same as the normal method, but this one can specify how many to go back
    @Nonnull
    public static AbstractInsnNode getPrevious(@Nonnull AbstractInsnNode insn, int count) {
        for(int i = 0; i < count; i++) insn = Objects.requireNonNull(insn.getPrevious());
        return insn;
    }

    //same as the normal method, but this one can specify how many to go forward
    @Nonnull
    public static AbstractInsnNode getNext(@Nonnull AbstractInsnNode insn, int count) {
        for(int i = 0; i < count; i++) insn = Objects.requireNonNull(insn.getNext());
        return insn;
    }

    //returns true if the insn is both a method and if it matches the name & desc
    public static boolean checkMethod(@Nullable AbstractInsnNode insn, @Nullable String name, @Nullable String desc) {
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
}
