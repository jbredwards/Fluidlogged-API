package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public class BlockStairsPlugin extends AbstractPlugin
{
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_176221_a" : "getActualState";
    }

    @Nullable
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKESTATIC && insn instanceof MethodInsnNode) {
            ((MethodInsnNode)insn).owner = "git/jbredwards/fluidlogged_api/asm/ASMHooks";
            ((MethodInsnNode)insn).name = "getShape";

            return true;
        }

        return false;
    }
}
