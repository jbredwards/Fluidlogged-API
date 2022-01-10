package git.jbredwards.fluidlogged_api.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * boat work with water FluidStates
 * @author jbred
 *
 */
public final class EntityBoatPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //getWaterLevelAbove, line 502
        if(checkMethod(method, obfuscated ? "func_184451_k" : "getWaterLevelAbove", "()F"))
            return 1;

        //checkInWater, line 613
        else if(checkMethod(method, obfuscated ? "func_184446_u" : "checkInWater", "()Z"))
            return 1;

        //getUnderwaterStatus, line 668
        else if(checkMethod(method, obfuscated ? "func_184444_v" : "getUnderwaterStatus", "()Lnet/minecraft/entity/item/EntityBoat$Status;"))
            return 1;

        //updateFallState, line 945
        else if(checkMethod(method, obfuscated ? "func_184231_a" : "updateFallState", "(DZLnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/BlockPos;)V"))
            return 1;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/common/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
