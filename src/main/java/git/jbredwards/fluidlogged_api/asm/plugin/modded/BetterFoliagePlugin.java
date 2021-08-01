package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * fluidlogged blocks are now effected by better foliage tweaks
 * @author jbred
 *
 */
public class BetterFoliagePlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //blockState
        if(ASMUtils.checkMethod(method, "blockState", "(Lmods/octarinecore/common/Int3;)Lnet/minecraft/block/state/IBlockState;")) return true;
        //blockData
        return ASMUtils.checkMethod(method, "blockData", "(Lmods/octarinecore/common/Int3;)Lmods/octarinecore/client/render/BlockData;");
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKEINTERFACE) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
