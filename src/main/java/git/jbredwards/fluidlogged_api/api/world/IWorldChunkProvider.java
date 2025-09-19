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
