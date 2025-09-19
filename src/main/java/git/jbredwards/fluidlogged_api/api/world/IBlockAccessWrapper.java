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

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * An IBlockAccess wrapper that implements {@link IChunkProvider}, {@link IFluidStateProvider}, and
 * {@link IWorldProvider}. This wrapper should be implemented by mods that add their own IBlockAccess wrappers, for
 * easy Fluidlogged API compatibility (see {@link net.minecraftforge.fml.common.Optional Forge's optional annotations}).
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public interface IBlockAccessWrapper extends IBlockAccess, IWorldChunkProvider
{
    /**
     * @return The wrapped IBlockAccess instance.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    IBlockAccess getWrapped();

    /**
     * Default implementation.
     *
     * @since 3.0.0
     * @author jbred
     *
     */
    class Impl implements IBlockAccessWrapper
    {
        @Nonnull
        public final IBlockAccess wrapped;
        public Impl(@Nonnull final IBlockAccess wrappedIn) { wrapped = wrappedIn; }

        @Nonnull
        @Override
        public IBlockAccess getWrapped() { return wrapped; }
    }

    // ============================
    // Wrapped IBlockAccess methods
    // ============================

    @Nonnull
    @Override
    default IBlockState getBlockState(@Nonnull final BlockPos pos) {
        return getWrapped().getBlockState(pos);
    }

    @Nullable
    @Override
    default TileEntity getTileEntity(@Nonnull final BlockPos pos) {
        return getWrapped().getTileEntity(pos);
    }

    @Override
    default int getStrongPower(@Nonnull final BlockPos pos, @Nonnull final EnumFacing direction) {
        return getWrapped().getStrongPower(pos, direction);
    }

    @Override
    default boolean isAirBlock(@Nonnull final BlockPos pos) {
        return getWrapped().isAirBlock(pos);
    }

    @Override
    default boolean isSideSolid(@Nonnull final BlockPos pos, @Nonnull final EnumFacing side, final boolean _default) {
        return getWrapped().isSideSolid(pos, side, _default);
    }

    @Nonnull
    @SideOnly(Side.CLIENT)
    @Override
    default Biome getBiome(@Nonnull final BlockPos pos) {
        return getWrapped().getBiome(pos);
    }

    @Nonnull
    @SideOnly(Side.CLIENT)
    @Override
    default WorldType getWorldType() {
        return getWrapped().getWorldType();
    }

    @SideOnly(Side.CLIENT)
    @Override
    default int getCombinedLight(@Nonnull final BlockPos pos, final int lightValue) {
        return getWrapped().getCombinedLight(pos, lightValue);
    }

    // ===================================
    // Wrapped IWorldChunkProvider methods
    // ===================================

    @Nullable
    @Override
    default Chunk getChunk(final int chunkX, final int chunkZ) {
        return getWrapped() instanceof IChunkProvider ? ((IChunkProvider)getWrapped()).getChunk(chunkX, chunkZ) : null;
    }

    @Nonnull
    @Override
    default FluidState getFluidState(final int x, final int y, final int z) {
        return getWrapped() instanceof IFluidStateProvider ? ((IFluidStateProvider)getWrapped()).getFluidState(x, y, z) : FluidState.EMPTY;
    }

    @Nonnull
    @Override
    default World getWorld() {
        return IWorldProvider.getWorld(getWrapped());
    }
}
