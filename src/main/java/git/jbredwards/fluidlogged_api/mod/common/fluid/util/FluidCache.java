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

package git.jbredwards.fluidlogged_api.mod.common.fluid.util;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.IBlockAccessWrapper;
import git.jbredwards.fluidlogged_api.api.world.ICubeData;
import git.jbredwards.fluidlogged_api.api.world.ICubeDataProvider;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public class FluidCache extends IBlockAccessWrapper.Impl
{
    @Nonnull public final BlockPos.MutableBlockPos mutablePos = new BlockPos.MutableBlockPos();
    @Nonnull protected final ICubeData[] cubes;

    public final int minX, minY, minZ, maxX, maxY, maxZ, cMinX, cMinY, cMinZ, cMaxX, cMaxY, cMaxZ;
    public FluidCache(@Nonnull final IBlockAccess accessIn, @Nonnull final Vec3i pos, final int radiusXZ, final int radiusY) {
        this(accessIn, pos.getX(), pos.getY(), pos.getZ(), radiusXZ, radiusY);
    }

    public FluidCache(@Nonnull final IBlockAccess accessIn, final int originX, final int originY, final int originZ, final int radiusXZ, final int radiusY) {
        this(accessIn, originX - radiusXZ, originX + radiusXZ, originY - radiusY, originY + radiusY, originZ - radiusXZ, originZ + radiusXZ);
    }

    public FluidCache(@Nonnull final IBlockAccess accessIn, final int minXIn, final int maxXIn, final int minYIn, final int maxYIn, final int minZIn, final int maxZIn) {
        super(accessIn);
        minX = minXIn;
        minY = minYIn;
        minZ = minZIn;
        maxX = maxXIn;
        maxY = maxYIn;
        maxZ = maxZIn;

        cMinX = minX >> 4;
        cMinY = minY >> 4;
        cMinZ = minZ >> 4;
        cMaxX = maxX >> 4;
        cMaxY = maxY >> 4;
        cMaxZ = maxZ >> 4;

        cubes = new ICubeData[(cMaxX - cMinX + 1) * (cMaxY - cMinY + 1) * (cMaxZ - cMinZ + 1)];
    }

    public FluidCache(@Nonnull final IBlockAccess accessIn, @Nonnull final AxisAlignedBB box) {
        this(accessIn, MathHelper.floor(box.minX), MathHelper.ceil(box.maxX), MathHelper.floor(box.minY), MathHelper.ceil(box.maxY), MathHelper.floor(box.minZ), MathHelper.ceil(box.maxZ));
    }

    @Nonnull
    public BlockPos.MutableBlockPos offset(@Nonnull final Vec3i origin, @Nonnull final Vec3i direction) {
        return offset(origin.getX(), origin.getY(), origin.getZ(), direction);
    }

    @Nonnull
    public BlockPos.MutableBlockPos offset(final int originX, final int originY, final int originZ, @Nonnull final Vec3i direction) {
        return mutablePos.setPos(originX + direction.getX(), originY + direction.getY(), originZ + direction.getZ());
    }

    @Nonnull
    public FluidState getFluidOrReal(@Nonnull final Vec3i pos) {
        return getFluidOrReal(pos.getX(), pos.getY(), pos.getZ());
    }

    @Nonnull
    public FluidState getFluidOrReal(final int x, final int y, final int z) {
        return FluidloggedUtils.getFluidState(this, mutablePos.setPos(x, y, z));
    }

    @Nonnull
    @Override
    public IBlockState getBlockState(@Nonnull final BlockPos pos) {
        return getBlockState(pos.getX(), pos.getY(), pos.getZ());
    }

    @Nonnull
    public IBlockState getBlockState(final int x, final int y, final int z) {
        @Nonnull final ICubeData cube = getCubeData(x >> 4, y >> 4, z >> 4);
        return cube != ICubeData.EMPTY ? cube.getBlockState(x, y, z) : super.getBlockState(mutablePos.setPos(x, y, z));
    }

    @Nullable
    @Override
    public Chunk getChunk(final int chunkX, final int chunkZ) {
        @Nullable final Chunk chunk = getCubeData(chunkX, cMinY, chunkZ).asChunk();
        return chunk != null ? chunk : super.getChunk(chunkX, chunkZ);
    }

    @Nonnull
    @Override
    public ICubeData getCubeData(final int chunkX, final int chunkY, final int chunkZ) {
        if(chunkX < cMinX || chunkX > cMaxX || chunkY < cMinY || chunkY > cMaxY || chunkZ < cMinZ || chunkZ > cMaxZ || !(wrapped instanceof ICubeDataProvider)) return ICubeData.EMPTY;
        final int index = getCubeIndex(chunkX, chunkY, chunkZ);
        return (cubes[index] != null ? cubes[index] : (cubes[index] = ((ICubeDataProvider)wrapped).getCubeData(chunkX, chunkY, chunkZ)));
    }

    protected int getCubeIndex(final int chunkX, final int chunkY, final int chunkZ) {
        return (chunkY - cMinY) * (cMaxZ - cMinZ + 1) * (cMaxX - cMinX + 1) + (chunkZ - cMinZ) * (cMaxX - cMinX + 1) + chunkX - cMinX;
    }

    @Nonnull
    @Override
    public FluidState getFluidState(final int x, final int y, final int z) {
        @Nonnull final ICubeData cube = getCubeData(x >> 4, y >> 4, z >> 4);
        return cube != ICubeData.EMPTY ? cube.getFluidState(x, y, z) : super.getFluidState(x, y, z);
    }

    @Nullable
    @Override
    public TileEntity getTileEntity(@Nonnull final BlockPos pos) {
        @Nonnull final World world = getWorld();
        if(world.processingLoadedTiles) {
            @Nullable final TileEntity pending = world.getPendingTileEntityAt(pos);
            if(pending != null) return pending;

            @Nonnull final ICubeData cube = getCubeData(pos);
            return cube != ICubeData.EMPTY ? cube.getTileEntity(pos) : null;
        }

        else {
            @Nonnull final ICubeData cube = getCubeData(pos);
            if(cube == ICubeData.EMPTY) return world.getPendingTileEntityAt(pos);

            @Nullable final TileEntity here = cube.getTileEntity(pos);
            return here != null ? here : world.getPendingTileEntityAt(pos);
        }
    }

    @Override
    public boolean isAirBlock(@Nonnull final BlockPos pos) {
        @Nonnull final IBlockState state = getBlockState(pos);
        return state.getBlock().isAir(state, this, pos);
    }
}
