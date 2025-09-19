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

package git.jbredwards.fluidlogged_api.mod.common.fluid.util.impl;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.ISpecializedFluidNeighborInfo;
import net.minecraft.block.Block;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;

/**
 *
 * @author jbred
 *
 */
public abstract class SpecializedFluidNeighborInfo extends FluidNeighborInfo implements ISpecializedFluidNeighborInfo
{
    public static class Forge extends SpecializedFluidNeighborInfo implements ISpecializedFluidNeighborInfo.Forge
    {
        public Forge(@Nonnull final IBlockAccess accessIn, @Nonnull final BlockPos originIn, @Nonnull final FluidState originStateIn, final int radius) {
            super(accessIn, originIn, originStateIn, radius);
        }
    }

    public static class Vanilla extends SpecializedFluidNeighborInfo implements ISpecializedFluidNeighborInfo.Vanilla
    {
        public Vanilla(@Nonnull final IBlockAccess accessIn, @Nonnull final BlockPos originIn, @Nonnull final FluidState originStateIn, final int radius) {
            super(accessIn, originIn, originStateIn, radius);
        }
    }

    // @Nonnull public final Boolean[] isFluidloggable;
    // @Nonnull public final Boolean[] isReplaceable;

    @Nullable public Boolean doFireTick;
    public SpecializedFluidNeighborInfo(@Nonnull final IBlockAccess accessIn, @Nonnull final BlockPos originIn, @Nonnull final FluidState originStateIn, final int radius) {
        super(accessIn, originIn, originStateIn, radius);
        // final int size = 16 * cache.diamXZ * cache.diamY * cache.diamXZ;
        // isFluidloggable = new Boolean[size];
        // isReplaceable = new Boolean[size];
    }

    @Nonnull
    @Override
    public Boolean setDoFireTick(@Nonnull final Boolean doFireTickIn) { return doFireTick = doFireTickIn; }

    // -------
    // getters
    // -------

    @Nullable
    @Override
    public Boolean getDoFireTick() { return doFireTick; }

    /*@Nonnull
    @Override
    public Boolean[] getIsFluidloggable() { return isFluidloggable; }

    @Nonnull
    @Override
    public Boolean[] getIsReplaceable() { return isReplaceable; }*/
}
