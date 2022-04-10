package git.jbredwards.fluidlogged_api.mod.asm.plugins;

import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * builds the internal ASMHooks functions
 * @author jbred
 *
 */
public final class PluginASMHooks implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals("getCanFluidFlow")) return 1;
        else if(method.name.equals("setCanFluidFlow")) return 2;
        else return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(index == 1 && insn.getOpcode() == ACONST_NULL) {
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraft/block/Block", "canFluidFlow", "Ljava/lang/Boolean;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 1);
            return true;
        }
        else if(index == 2 && insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new FieldInsnNode(PUTFIELD, "net/minecraft/block/Block", "canFluidFlow", "Ljava/lang/Boolean;"));
            setMaxLocals(method, 2);
            return true;
        }

        return false;
    }
}
