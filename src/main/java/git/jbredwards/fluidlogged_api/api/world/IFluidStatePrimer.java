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

import com.google.common.collect.Lists;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.world.chunk.ChunkPrimer;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * {@link ChunkPrimer} extends this at runtime, allows for more optimized FluidState world generation.
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public abstract class IFluidStatePrimer
{
    @Nonnull
    protected final List<FluidState> keys = Lists.newArrayList(FluidState.EMPTY);
    protected final char[] fluidData = new char[65536];

    /**
     * @param x X position inside the chunk.
     * @param y Y position inside the chunk.
     * @param z Z position inside the chunk.
     * @return The FluidState to be set at this position when the chunk generates.
     *
     * @throws ArrayIndexOutOfBoundsException If any of the following is true:
     * <blockquote><code>x < 0 || x > 15 || y < 0 || y > 255 || z < 0 || z > 15</code></blockquote>
     * @since 3.0.0
     */
    @Nonnull
    public FluidState getFluidState(final int x, final int y, final int z) {
        return keys.get(fluidData[ChunkPrimer.getBlockIndex(x, y, z)]);
    }

    /**
     * @param x X position inside the chunk.
     * @param y Y position inside the chunk.
     * @param z Z position inside the chunk.
     * @param fluidState The FluidState to be set.
     *
     * @throws ArrayIndexOutOfBoundsException If any of the following is true:
     * <blockquote><code>x < 0 || x > 15 || y < 0 || y > 255 || z < 0 || z > 15</code></blockquote>
     *
     * @throws NullPointerException If fluidState is null.
     */
    public void setFluidState(final int x, final int y, final int z, @Nonnull final FluidState fluidState) {
        if(fluidState.isEmpty()) fluidData[ChunkPrimer.getBlockIndex(x, y, z)] = 0;
        else {
            int indexedState = keys.indexOf(fluidState);
            if(indexedState == -1) {
                indexedState = keys.size();
                keys.add(fluidState);
            }

            fluidData[ChunkPrimer.getBlockIndex(x, y, z)] = (char)indexedState;
        }
    }

    /**
     * @param primer ChunkPrimer.
     * @return The provided ChunkPrimer as an IFluidStatePrimer.
     */
    @SuppressWarnings("DataFlowIssue") // ChunkPrimer is an instance of IFluidStatePrimer at runtime via asm
    @Nonnull
    public static IFluidStatePrimer of(@Nonnull final ChunkPrimer primer) { return (IFluidStatePrimer)(Object)primer; }
}
