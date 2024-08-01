/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.world;

import net.minecraft.world.chunk.Chunk;

import javax.annotation.Nullable;

/**
 * A convenience interface that extends both {@link IChunkProvider} and {@link IWorldProvider}.
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public interface IWorldChunkProvider extends IChunkProvider, IWorldProvider
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
    @Override
    default Chunk getChunk(final int chunkX, final int chunkZ) { return getWorld().getChunk(chunkX, chunkZ); }
}
