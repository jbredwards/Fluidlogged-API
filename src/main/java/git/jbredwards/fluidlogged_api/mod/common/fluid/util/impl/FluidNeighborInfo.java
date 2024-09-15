/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.fluid.util.impl;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.IFluidNeighborInfo;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public class FluidNeighborInfo implements IFluidNeighborInfo
{
    // properties cache
    // @Nonnull public final Boolean[] canFluidFlow;
    // @Nonnull public final Boolean[] isCompatibleFluid;

    @Nonnull public final FluidCache cache;
    @Nonnull public final FluidState originState;
    public final int originX, originY, originZ;

    public FluidNeighborInfo(@Nonnull final IBlockAccess accessIn, @Nonnull final BlockPos originIn, @Nonnull final FluidState originStateIn, final int radius) {
        if(accessIn instanceof FluidCache) { // use accessIn as cache, if it's both a cache and if the new radius can fit
            @Nonnull final FluidCache cacheIn = (FluidCache)accessIn;
            if(cacheIn.minX <= originIn.getX() - radius
            && cacheIn.maxZ >= originIn.getX() + radius
            && cacheIn.minY <= originIn.getY() - 1
            && cacheIn.maxY >= originIn.getY() + 1
            && cacheIn.minZ <= originIn.getZ() - radius
            && cacheIn.maxZ >= originIn.getZ() + radius) cache = cacheIn;
            else cache = new FluidCache(accessIn, originIn, radius, 1);
        }

        else cache = new FluidCache(accessIn, originIn, radius, 1);
        originState = originStateIn;
        originX = originIn.getX();
        originY = originIn.getY();
        originZ = originIn.getZ();

        // canFluidFlow = new Boolean[EnumFacing.VALUES.length * cache.diamXZ * cache.diamY * cache.diamXZ];
        // isCompatibleFluid = new Boolean[cache.diamXZ * cache.diamY * cache.diamXZ];
    }

    @Nonnull
    @Override
    public FluidCache getCache() { return cache; }

    @Nonnull
    @Override
    public FluidState getOrigin() { return originState; }

    @Override
    public int getOriginX() { return originX; }

    @Override
    public int getOriginY() { return originY; }

    @Override
    public int getOriginZ() { return originZ; }

    /*@Nonnull
    @Override
    public Boolean[] getCanFluidFlow() { return canFluidFlow; }*/

    /*@Nonnull
    @Override
    public Boolean[] getIsCompatibleFluid() { return isCompatibleFluid; }*/
}
