package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * get block fog color from possible FluidState
 * @author jbred
 *
 */
public final class PluginActiveRenderInfo implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_186703_a" : "getBlockStateAtEntityViewpoint"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getBlockStateAtEntityViewpoint: (changes are around line 82)
         * Old code:
         * IBlockState iblockstate = worldIn.getBlockState(blockpos);
         *
         * New code:
         * //use FluidState fog color rather than state here
         * IBlockState iblockstate = FluidloggedUtils.getFluidOrReal(worldIn, blockpos);
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return false;
        }
        /*
         * getBlockStateAtEntityViewpoint: (changes are around line 84)
         * Old code:
         * if (iblockstate.getMaterial().isLiquid())
         * {
         *     ...
         * }
         *
         * New code:
         * //skip built-in BlockLiquid special case, it's been moved to BlockLiquid directly
         * if (false)
         * {
         *     ...
         * }
         */
        else if(checkMethod(insn, obfuscated ? "func_76224_d" : "isLiquid")) {
            instructions.insert(new InsnNode(ICONST_0));
            removeFrom(instructions, insn, -2);
            return true;
        }

        return false;
    }
}
