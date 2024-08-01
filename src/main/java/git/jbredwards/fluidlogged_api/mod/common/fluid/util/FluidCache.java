/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.fluid.util;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.IBlockAccessWrapper;
import git.jbredwards.fluidlogged_api.api.world.IChunkProvider;
import git.jbredwards.fluidlogged_api.api.world.IFluidStateProvider;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.PluginWorld;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.EnumFacing;
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
    @Nonnull
    public final IBlockState[] states;

    @Nonnull
    protected final OptionalChunk[] chunks;
    protected static final class OptionalChunk
    {
        @Nullable
        public final Chunk chunk;
        public OptionalChunk(@Nullable final Chunk chunkIn) { chunk = chunkIn; }
    }

    @Nonnull
    public final BlockPos.MutableBlockPos mutablePos = new BlockPos.MutableBlockPos();
    public final int minX, minY, minZ, maxX, maxY, maxZ, cMinX, cMinZ, cMaxX, cMaxZ;

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
        cMinZ = minZ >> 4;
        cMaxX = maxX >> 4;
        cMaxZ = maxZ >> 4;

        states = new IBlockState[(maxXIn - minXIn + 1) * (maxYIn - minYIn + 1) * (maxZIn - minZIn + 1)];
        chunks = new OptionalChunk[(cMaxX - cMinX + 1) * (cMaxZ - cMinZ + 1)];
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
        return FluidloggedUtils.getFluidState(wrapped, mutablePos.setPos(x, y, z));
    }

    @Nonnull
    @Override
    public IBlockState getBlockState(@Nonnull final BlockPos pos) {
        return getBlockState(pos.getX(), pos.getY(), pos.getZ());
    }

    @Nonnull
    public IBlockState getBlockState(final int x, final int y, final int z) {
        if(x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ) {
            final int index = (y - minY) * (maxZ - minZ + 1) * (maxX - minX + 1) + (z - minZ) * (maxX - minX + 1) + x - minX;
            if(states[index] != null) return states[index];

            @Nullable final Chunk chunk = getChunk(x >> 4, z >> 4);
            return states[index] = (chunk != null ? chunk.getBlockState(x, y, z) : wrapped.getBlockState(mutablePos.setPos(x, y, z)));
        }

        @Nullable final Chunk chunk = getChunk(x >> 4, z >> 4);
        return chunk != null ? chunk.getBlockState(x, y, z) : wrapped.getBlockState(mutablePos.setPos(x, y, z));
    }

    @Nullable
    @Override
    public Chunk getChunk(final int chunkX, final int chunkZ) {
        if(chunkX < cMinX || chunkX > cMaxX || chunkZ < cMinZ || chunkZ > cMaxZ || !(wrapped instanceof IChunkProvider)) return null;
        final int index = (chunkZ - cMinZ) * (cMaxX - cMinX + 1) + chunkX - cMinX;
        return (chunks[index] != null ? chunks[index] : (chunks[index] = new OptionalChunk(((IChunkProvider)wrapped).getChunk(chunkX, chunkZ)))).chunk;
    }

    @Nonnull
    @Override
    public FluidState getFluidState(final int x, final int y, final int z) {
        if(!(wrapped instanceof IFluidStateProvider)) return FluidState.EMPTY;
        @Nullable final Chunk chunk = getChunk(x >> 4, z >> 4);
        return chunk != null ? FluidState.getFromProvider(chunk, x, y, z) : ((IFluidStateProvider)wrapped).getFluidState(x, y, z);
    }

    @Nullable
    @Override
    public TileEntity getTileEntity(@Nonnull final BlockPos pos) {
        @Nonnull final World world = getWorld();
        if(world.processingLoadedTiles) {
            @Nullable final TileEntity pending = world.getPendingTileEntityAt(pos);
            if(pending != null) return pending;

            @Nullable final Chunk chunk = getChunk(pos);
            return chunk != null ? chunk.getTileEntity(pos, Chunk.EnumCreateEntityType.IMMEDIATE) : null;
        }

        else {
            @Nullable final Chunk chunk = getChunk(pos);
            if(chunk == null) return world.getPendingTileEntityAt(pos);

            @Nullable final TileEntity here = chunk.getTileEntity(pos, Chunk.EnumCreateEntityType.IMMEDIATE);
            return here != null ? here : world.getPendingTileEntityAt(pos);
        }
    }

    @Override
    public int getStrongPower(@Nonnull final BlockPos pos, @Nonnull final EnumFacing direction) {
        return PluginWorld.Hooks.getStrongPower(this, pos, direction);
    }

    @Override
    public boolean isAirBlock(@Nonnull final BlockPos pos) {
        @Nonnull final IBlockState state = getBlockState(pos);
        return state.getBlock().isAir(state, this, pos);
    }
}
