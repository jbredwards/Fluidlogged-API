package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix BOP fluid block mixing
 * @author jbred
 *
 */
public final class PluginBiomesOPlenty implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("checkForMixing"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * checkForMixing:
         * Old code:
         * if (enumfacing != EnumFacing.DOWN && (worldIn.getBlockState(pos.offset(enumfacing)).getMaterial().isLiquid() == true))
         * {
         *     ...
         * }
         *
         * New code:
         * //check for the possibility of a neighboring FluidState making contact with this fluid
         * if (enumfacing != EnumFacing.DOWN && Hooks.canBOPFluidFlow(worldIn, pos, enumfacing))
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, obfuscated ? "func_76224_d" : "isLiquid")) {
            instructions.insert(insn, genMethodNode("canBOPFluidFlow", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"));
            removeFrom(instructions, insn, -3);
        }
        /*
         * checkForMixing
         * Old code:
         * if (worldIn.getBlockState(pos.offset(enumfacing)).getBlock() != this.getBlockState().getBlock())
         * {
         *     ...
         * }
         *
         * New code:
         * //check if there's actually a FluidState, and make sure it's not compatible with this
         * if (Hooks.getBOPFluidOrFalse(worldIn, pos.offset(facing), this.getDefaultState()) != this.getBlockState().getBlock())
         * {
         *     ...
         * }
         */
        //account for FluidState
        else if(checkMethod(insn.getNext(), obfuscated ? "func_177230_c" : "getBlock")) {
            instructions.insert(insn, genMethodNode("getBOPFluidOrAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, new MethodInsnNode(INVOKESPECIAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canBOPFluidFlow(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumFacing facing) {
            final IBlockState here = world.getBlockState(pos);
            return here.getBlock().isReplaceable(world, pos)
                    && FluidloggedUtils.canFluidFlow(world, pos, here, facing)
                    && FluidloggedUtils.canFluidFlow(world, pos.offset(facing), world.getBlockState(pos.offset(facing)), facing.getOpposite());
        }

        @Nonnull
        public static IBlockState getBOPFluidOrFalse(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
            final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos);
            return fluidState.isEmpty() ? state : fluidState.getState();
        }
    }
}
