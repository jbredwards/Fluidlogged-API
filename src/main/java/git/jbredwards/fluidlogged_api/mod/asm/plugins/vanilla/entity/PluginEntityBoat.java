package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * boats work with water FluidStates
 * @author jbred
 *
 */
public final class PluginEntityBoat implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, obfuscated ? "func_184451_k" : "getWaterLevelAbove", "()F")
                || checkMethod(method, obfuscated ? "func_184446_u" : "checkInWater", "()Z")
                || checkMethod(method, obfuscated ? "func_184444_v" : "getUnderwaterStatus", "()Lnet/minecraft/entity/item/EntityBoat$Status;")
                || checkMethod(method, obfuscated ? "func_184231_a" : "updateFallState", "(DZLnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/BlockPos;)V");
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getWaterLevelAbove, checkInWater, getUnderwaterStatus, updateFallState: (changes are around lines 502, 613, 668, and 945)
         * Old code:
         * IBlockState iblockstate = this.world.getBlockState(blockpos$pooledmutableblockpos);
         *
         * New code:
         * //account for FluidStates
         * IBlockState iblockstate = FluidloggedUtils.getFluidOrReal(this.world, blockpos$pooledmutableblockpos);
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
