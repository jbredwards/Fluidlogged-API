/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.IWorldProvider;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;

/**
 * use this if your block can be fluidlogged.
 * @author jbred
 *
 */
public interface IFluidloggable
{
    /**
     * @return true if the IBlockState is fluidloggable
     */
    default boolean isFluidloggable(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos) {
        return true;
    }

    /**
     * @return true if the IBlockState can be fluidlogged with the input fluid
     */
    default boolean isFluidValid(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final Fluid fluid) {
        return isFluidloggable(state, world, pos);
    }

    /**
     *
     * @param state
     * @param access
     * @param pos
     * @param fluidState
     * @return
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     */
    default boolean isFluidloggable(@Nonnull final IBlockState state, @Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
        @Nonnull final World world = IWorldProvider.getWorld(access);

        if(fluidState.isEmpty()) return isFluidloggable(state, world, pos); // basic check for if the state can ever be fluidlogged at all
        else if(!fluidState.isSource()) { // non-source blocks are only fluidloggable if this block is not solid on the bottom face
            return FluidloggedUtils.canFluidOccupy(state.getActualState(access, pos), access, pos, fluidState) && isFluidValid(state, world, pos, fluidState.getFluid());
        }

        return isFluidValid(state, world, pos, fluidState.getFluid());
    }

    /**
     * called by {@link FluidloggedUtils#canFluidFlow},
     * which is invoked a lot, so try to keep the code for this fairly light.
     *
     * @return true if the contained fluid can flow from the specified side,
     * or if a fluid can flow into this block from the specified side
     */
    default boolean canFluidFlow(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final EnumFacing side) {
        return here.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    /**
     * @return true if the FluidState should be visible while this is fluidlogged
     */
    @SideOnly(Side.CLIENT)
    default boolean shouldFluidRender(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final FluidState fluidState) {
        return true;
    }

    /**
     * called by {@link FluidloggedUtils#setFluidState}
     * when the stored FluidState is changed
     *
     * @return PASS - run & return {@link FluidloggedUtils#setFluidState_Internal},
     * FAIL - assume the change never happened,
     * SUCCESS - assume the change happened
     */
    @Nonnull
    default EnumActionResult onFluidChange(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final FluidState newFluid, final int blockFlags) {
        return newFluid.isEmpty() ? onFluidDrain(world, pos, here, blockFlags) : onFluidFill(world, pos, here, newFluid, blockFlags);
    }

    /**
     * convenience method called by {@link IFluidloggable#onFluidChange} when a new FluidState is put here
     */
    @Nonnull
    default EnumActionResult onFluidFill(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final FluidState newFluid, final int blockFlags) {
        return EnumActionResult.PASS;
    }

    /**
     * convenience method called by {@link IFluidloggable#onFluidChange} when the stored FluidState is removed
     */
    @Nonnull
    default EnumActionResult onFluidDrain(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, final int blockFlags) {
        return EnumActionResult.PASS;
    }

    /**
     * @return
     * @throws NullPointerException If state is null.
     *
     * @since 3.0.0
     * @author jbred
     */
    default boolean overrideApplyDefaultsSetting() { return false; }
}
