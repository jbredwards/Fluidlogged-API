package git.jbredwards.fluidlogged_api.common.block;

import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.eventhandler.Event;
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
    default boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable Fluid fluid, @Nonnull EnumFacing side) {
        return state.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    //returns true if the FluidState should be visible while this is fluidlogged
    @SideOnly(Side.CLIENT)
    default boolean shouldFluidRender(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Fluid fluid) {
        return fluid.canBePlacedInWorld() && fluid.getBlock().getDefaultState().getRenderType() == EnumBlockRenderType.MODEL;
    }

    //ran when FluidloggedUtils#setFluidState gets called into this
    //DEFAULT = run & return result of the default change action
    //DENY    = think the change never happened
    //ALLOW   = think the change happened
    @Nonnull
    default Event.Result onFluidChange(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable Fluid oldFluid, @Nullable Fluid newFluid, int flags) {
        return newFluid == null ? onFluidDrain(world, pos, state, oldFluid, flags) : onFluidFill(world, pos, state, oldFluid, newFluid, flags);
    }

    //called by IFluidloggable#onFluidChange
    //DEFAULT = run & return result of the default fill action
    //DENY    = think the fill never happened
    //ALLOW   = think the fill happened
    @Nonnull
    default Event.Result onFluidFill(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable Fluid oldFluid, @Nonnull Fluid newFluid, int flags) {
        return Event.Result.DEFAULT;
    }

    //called by IFluidloggable#onFluidChange
    //DEFAULT = run & return result of the default drain action
    //DENY    = think the drain never happened
    //ALLOW   = think the drain happened
    @Nonnull
    default Event.Result onFluidDrain(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable Fluid oldFluid, int flags) {
        return Event.Result.DEFAULT;
    }
}
