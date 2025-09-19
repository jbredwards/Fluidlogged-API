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

package git.jbredwards.fluidlogged_api.api.datafix;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 * Used alongside {@link IFluidloggedDataMapper} to split data for "pseudo-fluidlogged" blocks into
 * real {@link FluidState FluidState data}. This class holds the new data to be set in the chunk, and
 * also allows the block itself to be changed (if desired).
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public class FluidMappingData
{
    @Nullable
    public Block block;
    public int meta = -1;

    @Nonnull
    public FluidState fluidState;
    public FluidMappingData(@Nonnull final FluidState fluidStateIn) {
        fluidState = Objects.requireNonNull(fluidStateIn);
    }

    /**
     * If this is not set, the existing block will be used.
     * @param blockIn The block to use (not the FluidState).
     * @return This FluidMappingData, for ease of use.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public FluidMappingData withBlock(@Nonnull final Block blockIn) {
        block = Objects.requireNonNull(blockIn);
        return this;
    }

    /**
     * If this is not set, the existing block metadata will be used.
     * @param metaIn The block metadata to use (not the FluidState).
     * @return This FluidMappingData, for ease of use.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public FluidMappingData withMetadata(final int metaIn) {
        meta = metaIn;
        return this;
    }
}
