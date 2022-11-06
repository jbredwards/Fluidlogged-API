package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * sugar cane blocks now recognise water FluidStates
 * @author jbred
 *
 */
public final class PluginBlockReed implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_176196_c" : "canPlaceBlockAt"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * canPlaceBlockAt: (changes are around line 103)
         * Old code:
         * IBlockState iblockstate = worldIn.getBlockState(blockpos.offset(enumfacing));
         *
         * New code:
         * //check for FluidState
         * IBlockState iblockstate = FluidloggedUtils.getFluidOrReal(worldIn, blockpos.offset(enumfacing));
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState") && checkMethod(insn.getPrevious(), obfuscated ? "func_177972_a" : "offset")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
