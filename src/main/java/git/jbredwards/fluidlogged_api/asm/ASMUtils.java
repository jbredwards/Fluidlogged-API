package git.jbredwards.fluidlogged_api.asm;

import org.objectweb.asm.tree.*;

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

    //same as above, but for method nodes
    public static boolean checkMethod(@Nonnull MethodNode method, @Nullable String name, @Nullable String desc) {
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
    public static boolean checkField(@Nullable AbstractInsnNode insn, @Nullable String name, @Nullable String desc) {
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
}
