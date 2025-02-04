/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.capability;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import it.unimi.dsi.fastutil.chars.CharSet;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;

import javax.annotation.Nonnull;

/**
 * Stores FluidStates during runtime. All methods in this interface should only be used internally! If you're a modder,
 * see {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils FluidloggedUtils}.
 *
 * @since 1.9.0
 * @author jbred
 *
 */
public interface IFluidStateContainer extends IPosSerializer
{
    /**
     * @param serializedPos Serialized position. Important note: serialization of the pos for this method
     *                      is handled by {@link IPosSerializer}, <b>not {@link BlockPos#toLong()}.</b>
     * @return true if this has a non-empty fluid state at the position.
     *
     * @since 1.9.0
     * @author jbred
     */
    boolean hasFluidState(final char serializedPos);

    /**
     * This method should only be used internally! If you're a modder, use
     * {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils#getFluidState(IBlockAccess, BlockPos, IBlockState) FluidloggedUtils::getBlockState}
     * instead.
     *
     * @param serializedPos Serialized position. Important note: serialization of the pos for this method
     *                      is handled by {@link IPosSerializer}, <b>not {@link BlockPos#toLong()}.</b>
     * @param fallback The fluid state to return if none is present at the provided pos.
     *                 This value should typically be set to {@link FluidState#EMPTY}.
     * @return The fluid state at the position, or fallback if none is present.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    FluidState getFluidState(final char serializedPos, @Nonnull final FluidState fallback);

    /**
     * This method should only be used internally! If you're a modder, use
     * {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils#getFluidState(IBlockAccess, BlockPos, IBlockState) FluidloggedUtils::getBlockState}
     * instead.
     *
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @param fallback The fluid state to return if none is present at the provided pos.
     *                 This value should typically be set to {@link FluidState#EMPTY}.
     * @return The fluid state at the position, or fallback if none is present.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    default FluidState getFluidState(final int x, final int y, final int z, @Nonnull final FluidState fallback) {
        return getFluidState(serializePos(x, y, z), fallback);
    }

    /**
     * This method should only be used internally! If you're a modder, use
     * {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils#getFluidState(IBlockAccess, BlockPos, IBlockState) FluidloggedUtils::getBlockState}
     * instead.
     *
     * @param pos Position.
     * @param fallback The fluid state to return if none is present at the provided pos.
     *                 This value should typically be set to {@link FluidState#EMPTY}.
     * @return The fluid state at the position, or fallback if none is present.
     *
     * @throws NullPointerException If pos is null.
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default FluidState getFluidState(@Nonnull final BlockPos pos, @Nonnull final FluidState fallback) {
        return getFluidState(pos.getX(), pos.getY(), pos.getZ(), fallback);
    }

    /**
     * Sets the fluid state at the provided position. This method should only be used internally! If you're a modder, use
     * {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils#setFluidState(World, BlockPos, IBlockState, FluidState, boolean, int) FluidloggedUtils::setBlockState}
     * instead.
     *
     * @param serializedPos Serialized position. Important note: serialization of the pos for this method
     *                      is handled by {@link IPosSerializer}, <b>not {@link BlockPos#toLong()}.</b>
     * @param fluidState FluidState to be set.
     * @return True if fluidState was set, false otherwise.
     *
     * @throws NullPointerException If fluidState is null.
     * @since 3.0.0
     * @author jbred
     */
    boolean setFluidState(final char serializedPos, @Nonnull final FluidState fluidState);

    /**
     * Sets the fluid state at the provided position. This method should only be used internally! If you're a modder, use
     * {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils#setFluidState(World, BlockPos, IBlockState, FluidState, boolean, int) FluidloggedUtils::setBlockState}
     * instead.
     *
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @param fluidState FluidState to be set.
     * @return True if fluidState was set, false otherwise.
     *
     * @throws NullPointerException If fluidState is null.
     * @since 3.0.0
     * @author jbred
     */
    default boolean setFluidState(final int x, final int y, final int z, @Nonnull final FluidState fluidState) {
        return setFluidState(serializePos(x, y, z), fluidState);
    }

    /**
     * Sets the fluid state at the provided position. This method should only be used internally! If you're a modder, use
     * {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils#setFluidState(World, BlockPos, IBlockState, FluidState, boolean, int) FluidloggedUtils::setBlockState}
     * instead.
     *
     * @param pos Position.
     * @param fluidState FluidState to be set.
     * @return True if fluidState was set, false otherwise.
     *
     * @throws NullPointerException If pos or fluidState are null.
     * @since 3.0.0
     * @author jbred
     */
    default boolean setFluidState(@Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
        return setFluidState(pos.getX(), pos.getY(), pos.getZ(), fluidState);
    }

    /**
     * Clears all fluid states from this container.
     *
     * @since 1.9.0
     * @author jbred
     */
    void clearFluidStates();

    /**
     * @return An unmodifiable set containing all the serialized positions that have non-empty FluidStates.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    CharSet getSerializedPositions();

    /**
     * Performs the provided action for each non-empty FluidState in this container.
     * @param action Action to perform.
     *
     * @since 3.0.0
     * @author jbred
     */
    void forEach(@Nonnull final ContainerAction action);
    interface ContainerAction
    {
        /**
         * Performs this operation on the given arguments.
         * @param serializedPos The position input.
         * @param fluidState The fluid state input.
         */
        void accept(final char serializedPos, @Nonnull final FluidState fluidState);
    }
}
