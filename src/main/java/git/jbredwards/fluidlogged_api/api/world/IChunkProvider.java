/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.world;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.chunk.Chunk;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Gives IBlockAccess instances the option to provide chunks in a way that this mod can access them.
 * <p>
 * {@link net.minecraft.world.World World} and {@link net.minecraft.world.ChunkCache ChunkCache} implement this at runtime, along with some modded classes.
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public interface IChunkProvider extends IFluidStateProvider
{
    /**
     * @param chunkX X position of the chunk (chunk coords, not block coords).
     * @param chunkZ Z position of the chunk (chunk coords, not block coords).
     * @return The chunk at the position.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nullable
    Chunk getChunk(final int chunkX, final int chunkZ);

    /**
     * @param pos Position.
     * @return The chunk at the position.
     * @throws NullPointerException If pos is null.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nullable
    default Chunk getChunk(@Nonnull final Vec3i pos) { return getChunk(pos.getX() >> 4, pos.getZ() >> 4); }

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
    @Override
    default FluidState getFluidState(final int x, final int y, final int z) {
        return FluidState.getFromProvider(getChunk(x >> 4, z >> 4), x, y, z);
    }
}
