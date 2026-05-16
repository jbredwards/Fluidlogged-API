/*
 * Copyright (C) <2026 to Present> <jbredwards>
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

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.PluginChunk;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.chunk.Chunk;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * An abstract representation of block data from a slice in the world.
 *
 * @since 3.3.0
 * @see ICubeDataProvider
 * @author jbred
 *
 */
public interface ICubeData
{
    /**
     * An {@code ICubeData} instance that contains no data. Always used instead of a null value.
     *
     * @since 3.3.0
     */
    @Nonnull
    ICubeData EMPTY = new ICubeData() {
        @Nullable
        @Override
        public Chunk asChunk() { return null; }

        @Nonnull
        @Override
        public IBlockState getBlockState(final int x, final int y, final int z) { return Blocks.AIR.getDefaultState(); }

        @Nonnull
        @Override
        public FluidState getFluidState(final int x, final int y, final int z) { return FluidState.EMPTY; }

        @Nullable
        @Override
        public TileEntity getTileEntity(final int x, final int y, final int z) { return null; }
    };

    /**
     * @param access IBlockAccess.
     * @param pos Position.
     * @return A 16x16x16 view of the provided IBlockAccess encompassing the given coords.
     * @throws NullPointerException If pos is null.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    static ICubeData get(@Nullable final IBlockAccess access, @Nonnull final Vec3i pos) {
        return access instanceof ICubeDataProvider ? ((ICubeDataProvider)access).getCubeData(pos) : EMPTY;
    }

    /**
     * @param chunk Chunk.
     * @param chunkY Y position of the cube (chunk coords, not block coords).
     * @return A 16x16x16 cube that's aligned with the provided chunk.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    static ICubeData getFromChunk(@Nullable final Chunk chunk, final int chunkY) {
        return chunk == null ? EMPTY : PluginChunk.Hooks.getCubeData(chunk, chunkY);
    }

    /**
     * @return This ICubeData instance as a Chunk, or null if not based from a Chunk.
     * @since 3.3.0
     * @author jbred
     */
    @Nullable
    Chunk asChunk();

    /**
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @return The block state at the position, or {@code Blocks.AIR.getDefaultState()} if none is
     * present at the position or if the position is outside of this cube.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    IBlockState getBlockState(final int x, final int y, final int z);

    /**
     * @param pos Position.
     * @return The block state at the position, or {@code Blocks.AIR.getDefaultState()} if none is
     * present at the position or if the position is outside of this cube.
     * @throws NullPointerException If pos is null.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    default IBlockState getBlockState(@Nonnull final Vec3i pos) {
        return getBlockState(pos.getX(), pos.getY(), pos.getZ());
    }

    /**
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @return The fluid state at the position, or {@link FluidState#EMPTY} if none is present
     * at the position or if the position is outside of this cube.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    FluidState getFluidState(final int x, final int y, final int z);

    /**
     * @param pos Position.
     * @return The fluid state at the position, or {@link FluidState#EMPTY} if none is present
     * at the position or if the position is outside of this cube.
     * @throws NullPointerException If pos is null.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    default FluidState getFluidState(@Nonnull final Vec3i pos) {
        return getFluidState(pos.getX(), pos.getY(), pos.getZ());
    }

    /**
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @return The tile entity at the position, or null if none is present
     * at the position or if the position is outside of this cube.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nullable
    TileEntity getTileEntity(final int x, final int y, final int z);

    /**
     * @param pos Position.
     * @return The tile entity at the position, or null if none is present
     * at the position or if the position is outside of this cube.
     * @throws NullPointerException If pos is null.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nullable
    default TileEntity getTileEntity(@Nonnull final Vec3i pos) {
        return getTileEntity(pos.getX(), pos.getY(), pos.getZ());
    }
}
