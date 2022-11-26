package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * water FluidStates are now seen as water blocks
 * @author jbred
 *
 */
public final class PluginRandomPositionGenerator implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_191380_b" : "isWaterDestination"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * isWaterDestination: (changes are around line 179)
         * Old code:
         * return p_191380_1_.world.getBlockState(p_191380_0_).getMaterial() == Material.WATER;
         *
         * New code:
         * //account for FluidStates
         * return FluidloggedUtils.getFluidOrReal(p_191380_1_.world, p_191380_0_).getMaterial() == Material.WATER;
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
