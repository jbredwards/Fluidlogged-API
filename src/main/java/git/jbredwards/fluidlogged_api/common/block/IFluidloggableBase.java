package git.jbredwards.fluidlogged_api.common.block;

import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * some fundamental code that should be shared across every block that's related to fluidlogging
 * @author jbred
 *
 */
public interface IFluidloggableBase
{
    //returns true if the fluid should be visible while this is fluidlogged
    @SideOnly(Side.CLIENT)
    default boolean doesFluidRenderInternal(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Fluid fluid) { return true; }

    //returns true if the fluid should be visible while this is fluidlogged
    @SideOnly(Side.CLIENT)
    static boolean doesFluidRender(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Fluid fluid) {
        return !(state.getBlock() instanceof IFluidloggableBase) || ((IFluidloggableBase)state.getBlock()).doesFluidRenderInternal(world, pos, state, fluid);
    }

    //returns true if the contained fluid can flow from the specified side
    default boolean canFluidFlowInternal(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable Fluid fluid, @Nonnull EnumFacing side) {
        return state.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    //returns true if the contained fluid can flow from the specified side
    static boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable Fluid fluid, @Nonnull EnumFacing side) {
        if(state.getBlock() instanceof IFluidloggableBase) return ((IFluidloggableBase)state.getBlock()).canFluidFlowInternal(world, pos, state, fluid, side);
        else return state.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }
}
