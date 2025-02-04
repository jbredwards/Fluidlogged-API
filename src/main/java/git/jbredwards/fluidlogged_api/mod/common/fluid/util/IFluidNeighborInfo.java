/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.fluid.util;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;

import javax.annotation.Nonnull;

/**
 * Creates and caches neighbor info as needed.
 *
 * @author jbred
 *
 */
public interface IFluidNeighborInfo
{
    // -----------
    // origin info
    // -----------

    @Nonnull
    FluidState getOrigin();
    int getOriginX();
    int getOriginY();
    int getOriginZ();

    // --------------------
    // conversion functions
    // --------------------

    @Nonnull
    default BlockPos getPosI(final int x, final int y, final int z) {
        return getPosIB(getXI(x), getYI(y), getZI(z));
    }

    @Nonnull
    default BlockPos getPosIB(final int xi, final int yi, final int zi) {
        return getCache().mutablePos.setPos(getXIB(xi), getYIB(yi), getZIB(zi));
    }

    // origin-relative to block
    default int getXB(final int x) { return getOriginX() + x; }
    default int getYB(final int y) { return getOriginY() - y * getOrigin().getDensityDir(); }
    default int getZB(final int z) { return getOriginZ() + z; }

    // origin-relative to index
    default int getXI(final int x) { return getOriginX() + x - getCache().minX; }
    default int getYI(final int y) { return getOriginY() + y - getCache().minY; }
    default int getZI(final int z) { return getOriginZ() + z - getCache().minZ; }

    // index to block
    default int getXIB(final int xi) { return xi + getCache().minX; }
    default int getYIB(final int yi) { return getYB(yi + getCache().minY - getOriginY()); }
    default int getZIB(final int zi) { return zi + getCache().minZ; }

    // ----------------
    // properties cache
    // ----------------

    @Nonnull
    FluidCache getCache();

    //@Nonnull
    //Boolean[] getCanFluidFlow();

    //@Nonnull
    //Boolean[] getIsCompatibleFluid();

    // ------------------------
    // index-relative functions
    // ------------------------

    @Nonnull
    default IBlockState getBlockStateI(final int xi, final int yi, final int zi) {
        return getCache().getBlockState(getXIB(xi), getYIB(yi), getZIB(zi));
    }

    @Nonnull
    default FluidState getFluidStateI(final int xi, final int yi, final int zi) {
        return getCache().getFluidOrReal(getXIB(xi), getYIB(yi), getZIB(zi));
    }

    default boolean canFluidFlowI(final int xi, final int yi, final int zi, @Nonnull final EnumFacing side) {
        /*final int index = side.getIndex() * getIsCompatibleFluid().length + getCache().getIndexI(xi, yi, zi);
        return getCanFluidFlow()[index] != null ? getCanFluidFlow()[index] : (getCanFluidFlow()[index] =*/ return
                FluidloggedUtils.canFluidFlow(getCache(), getPosIB(xi, yi, zi), getCache().getBlockState(getCache().mutablePos), side);//);
    }

    default boolean isCompatibleFluidI(final int xi, final int yi, final int zi) {
        /*final int index = getCache().getIndexI(xi, yi, zi);
        return getIsCompatibleFluid()[index] != null ? getIsCompatibleFluid()[index] : (getIsCompatibleFluid()[index] =*/ return
                FluidloggedUtils.isCompatibleFluid(getOrigin(), getFluidStateI(xi, yi, zi));//);
    }

    // -------------------------
    // origin-relative functions
    // -------------------------

    @Nonnull
    default IBlockState getBlockState(final int x, final int y, final int z) {
        return getCache().getBlockState(getXB(x), getYB(y), getZB(z));
    }

    @Nonnull
    default FluidState getFluidState(final int x, final int y, final int z) {
        return getCache().getFluidOrReal(getXB(x), getYB(y), getZB(z));
    }

    default boolean canFluidFlow(final int x, final int y, final int z, @Nonnull final EnumFacing side) {
        return canFluidFlowI(getXI(x), getYI(y), getZI(z), side);
    }

    default boolean isCompatibleFluid(final int x, final int y, final int z) {
        return isCompatibleFluidI(getXI(x), getYI(y), getZI(z));
    }
}
