/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
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
 * Have your block implement this if it can be fluidlogged.
 * <p>Use {@link net.minecraftforge.fml.common.Optional Forge's Optional @interfaces} to prevent a required Fluidlogged API dependency.</p>
 *
 * @since 1.7.0
 * @author jbred
 *
 */
public interface IFluidloggable
{
    /**
     * Called by {@link IFluidloggable#isFluidloggable(IBlockState, IBlockAccess, BlockPos, FluidState)}.
     *
     * @param state IBlockState to test.
     * @param world World of the IBlockState.
     * @param pos Position of the IBlockState.
     * @return True if the IBlockState is fluidloggable.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.8.0
     * @author jbred
     */
    default boolean isFluidloggable(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos) {
        return true;
    }

    /**
     * Called by {@link IFluidloggable#isFluidloggable(IBlockState, IBlockAccess, BlockPos, FluidState)}.
     *
     * @param state IBlockState to test.
     * @param world World of the IBlockState.
     * @param pos Position of the IBlockState.
     * @param fluid Fluid to test.
     * @return True if the IBlockState can be fluidlogged with the input fluid.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.8.0
     * @author jbred
     */
    default boolean isFluidValid(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final Fluid fluid) {
        return isFluidloggable(state, world, pos);
    }

    /**
     * @param state IBlockState to test.
     * @param access World of the IBlockState.
     * @param pos Position of the IBlockState.
     * @param fluidState FluidState to test, may be empty.
     * @return True if the IBlockState can be fluidlogged with the input FluidState.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 3.0.0
     * @author jbred
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
     * Called by {@link FluidloggedUtils#canFluidFlow},
     * which is invoked a lot, so try to keep the code for this fairly light.
     *
     * @param world World.
     * @param pos Position.
     * @param here IBlockState at the position.
     * @param side Side to flow from.
     * @return True if the contained fluid can flow from the specified side,
     * or if a fluid can flow into this block from the specified side.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 1.8.0
     * @author jbred
     */
    default boolean canFluidFlow(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final EnumFacing side) {
        return here.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    /**
     * @param world IBlockAccess.
     * @param pos Position.
     * @param here IBlockState at the position.
     * @param fluidState FluidState at the position.
     * @return True if the FluidState should be visible while this is fluidlogged.
     *
     * @throws NullPointerException If parameters are null.
     * @since 1.8.0
     * @author jbred
     */
    @SideOnly(Side.CLIENT)
    default boolean shouldFluidRender(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final FluidState fluidState) {
        return true;
    }

    /**
     * Called by {@link FluidloggedUtils#setFluidState} when the stored FluidState is to be changed.
     *
     * @param world World.
     * @param pos Position.
     * @param here IBlockState at the position.
     * @param newFluid FluidState to be set at the position, may be empty.
     * @param blockFlags The flags used when calling {@link FluidloggedUtils#setFluidState(World, BlockPos, IBlockState, FluidState, boolean, int)}.
     * @return PASS - Run & return {@link FluidloggedUtils#setFluidState_Internal}, FAIL - Assume the change never happened, SUCCESS - Assume the change happened.
     *
     * @throws NullPointerException If parameters are null.
     * @since 1.8.0
     * @author jbred
     */
    @Nonnull
    default EnumActionResult onFluidChange(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final FluidState newFluid, final int blockFlags) {
        return newFluid.isEmpty() ? onFluidDrain(world, pos, here, blockFlags) : onFluidFill(world, pos, here, newFluid, blockFlags);
    }

    /**
     * Convenience method called by {@link IFluidloggable#onFluidChange} when a new FluidState is to be set.
     *
     * @param world World.
     * @param pos Position.
     * @param here IBlockState at the position.
     * @param newFluid FluidState to be set at the position, is never empty.
     * @param blockFlags The flags used when calling {@link FluidloggedUtils#setFluidState(World, BlockPos, IBlockState, FluidState, boolean, int)}.
     * @return PASS - Run & return {@link FluidloggedUtils#setFluidState_Internal}, FAIL - Assume the change never happened, SUCCESS - Assume the change happened.
     *
     * @throws NullPointerException If parameters are null.
     * @since 1.8.0
     * @author jbred
     */
    @Nonnull
    default EnumActionResult onFluidFill(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final FluidState newFluid, final int blockFlags) {
        return EnumActionResult.PASS;
    }

    /**
     * Convenience method called by {@link IFluidloggable#onFluidChange} when the stored FluidState is to be removed.
     *
     * @param world World.
     * @param pos Position.
     * @param here IBlockState at the position.
     * @param blockFlags The flags used when calling {@link FluidloggedUtils#setFluidState(World, BlockPos, IBlockState, FluidState, boolean, int)}.
     * @return PASS - Run & return {@link FluidloggedUtils#setFluidState_Internal}, FAIL - Assume the change never happened, SUCCESS - Assume the change happened.
     *
     * @throws NullPointerException If parameters are null.
     * @since 1.8.0
     * @author jbred
     */
    @Nonnull
    default EnumActionResult onFluidDrain(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, final int blockFlags) {
        return EnumActionResult.PASS;
    }

    /**
     * @return True if this block should not have its fluidlogging functionality disabled while "applyDefaults" is turned off.
     *
     * @since 3.0.0
     * @author jbred
     */
    default boolean overrideApplyDefaultsSetting() { return false; }
}
