package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fishhook entities generate the fishing particles at water FluidStates
 * @author jbred
 *
 */
public final class PluginEntityFishHook implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return method.name.equals(obfuscated ? "func_70071_h_" : "onUpdate")
                || method.name.equals(obfuscated ? "func_190621_a" : "catchingFish");
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * onUpdate & cachingFish: (changes are around lines 174, 428, and 478)
         * Old code:
         * IBlockState iblockstate = this.world.getBlockState(blockpos);
         *
         * New code:
         * //account for FluidStates
         * IBlockState iblockstate = FluidloggedUtils.getFluidOrReal(this.world, blockpos);
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
        }

        return false;
    }
}
