package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * farmland blocks now recognise water FluidStates
 * @author jbred
 *
 */
public final class PluginBlockFarmland implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_176530_e" : "hasWater"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * hasWater: (changes are around line 119)
         * Old code:
         * if (worldIn.getBlockState(blockpos$mutableblockpos).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         *
         * New code:
         * //check for fluidlogged blocks
         * if (FluidloggedUtils.getFluidOrReal(worldIn, blockpos$mutableblockpos).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
