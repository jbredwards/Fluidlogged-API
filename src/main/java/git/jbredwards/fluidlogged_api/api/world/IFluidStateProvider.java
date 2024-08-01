/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.world;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.util.math.Vec3i;

import javax.annotation.Nonnull;

/**
 * Gives IBlockAccess instances the option to provide FluidStates in a way that this mod can access them.
 * If your IBlockAccess instance can provide chunks, implement {@link IChunkProvider} instead.
 * <p>
 * {@link net.minecraft.world.World World} and {@link net.minecraft.world.ChunkCache ChunkCache} implement this at runtime, along with some modded classes.
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public interface IFluidStateProvider
{
    /**
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @return The fluid state at the position, or {@link FluidState#EMPTY} if none is present.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    FluidState getFluidState(final int x, final int y, final int z);

    /**
     * @param pos Position.
     * @return The fluid state at the position, or {@link FluidState#EMPTY} if none is present.
     * @throws NullPointerException If pos is null.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    default FluidState getFluidState(@Nonnull final Vec3i pos) {
        return getFluidState(pos.getX(), pos.getY(), pos.getZ());
    }
}
