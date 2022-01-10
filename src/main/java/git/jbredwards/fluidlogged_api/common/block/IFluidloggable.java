package git.jbredwards.fluidlogged_api.common.block;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * implement this if your block can be fluidlogged
 * quick reminder that all interfaces support the forge net.minecraftforge.fml.common.Optional @interfaces!
 * (using them allows your blocks to support fluidlogging while making this mod only an optional dependency!)
 * @author jbred
 *
 */
public interface IFluidloggable
{
    //return true if this is a fluidloggable block
    default boolean isFluidloggable(@Nonnull IBlockState state) { return true; }

    //return true if this is fluidloggable with the input fluid
    default boolean isFluidValid(@Nonnull IBlockState state, @Nonnull Fluid fluid) { return isFluidloggable(state); }

    //has two purposes:
    //1: returns true if the contained fluid can flow from the specified side
    //2: returns true if a fluid can flow into this block from the specified side
    default boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nullable Fluid fluid, @Nonnull EnumFacing side) {
        return here.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    //returns true if the FluidState should be visible while this is fluidlogged
    @SideOnly(Side.CLIENT)
    default boolean shouldFluidRender(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull FluidState fluidState) {
        return !fluidState.isEmpty() && fluidState.getState().getRenderType() == EnumBlockRenderType.MODEL;
    }

    //called by FluidloggedUtils#setFluidState when the stored FluidState is changed
    //PASS = run & return FluidloggedUtils#setFluidState_Internal
    //FAIL = assume the change never happened
    //SUCCESS = assume the change happened
    @Nonnull
    default EnumActionResult onFluidChange(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull FluidState newFluid, int blockFlags) {
        return newFluid.isEmpty() ? onFluidDrain(world, pos, here, blockFlags) : onFluidFill(world, pos, here, newFluid, blockFlags);
    }

    //convenience method called by IFluidloggable#onFluidChange when a new FluidState is put here
    @Nonnull
    default EnumActionResult onFluidFill(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull FluidState newFluid, int blockFlags) {
        return EnumActionResult.PASS;
    }

    //convenience method called by IFluidloggable#onFluidChange when the stored FluidState is removed
    @Nonnull
    default EnumActionResult onFluidDrain(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, int blockFlags) {
        return EnumActionResult.PASS;
    }
}
