package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * armor stands don't remove fluids at their position when placed
 * @author jbred
 *
 */
public final class PluginItemArmorStand implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_180614_a" : "onItemUse"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * onItemUse: (changes are around lines 71-71)
         * Old code:
         * worldIn.setBlockToAir(blockpos);
         * worldIn.setBlockToAir(blockpos1);
         *
         * New code:
         * armor stands don't remove fluids at their position when placed
         * Hooks.setBlockToAirIfNonFluid(worldIn, blockpos);
         * Hooks.setBlockToAirIfNonFluid(worldIn, blockpos1);
         */
        if(checkMethod(insn.getPrevious(), obfuscated ? "func_175698_g" : "setBlockToAir")) {
            final boolean isDone = getNext(insn, 3).getOpcode() == NEW;
            instructions.insert(insn, genMethodNode("setBlockToAirIfNonFluid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V"));
            removeFrom(instructions, insn, -1);
            return isDone;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void setBlockToAirIfNonFluid(@Nonnull World world, @Nonnull BlockPos pos) {
            if(!FluidloggedUtils.isFluid(world.getBlockState(pos))) world.setBlockToAir(pos);
        }
    }
}
