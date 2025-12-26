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

import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.impl.SpecializedFluidNeighborInfo;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.chunk.BlockStateContainer;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.BlockFluidFinite;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.function.IntUnaryOperator;

/**
 *
 * @author jbred
 *
 */
public interface IFluidUpdateHelper extends ISpecializedFluidNeighborInfo
{
    int DEFAULT_COST = 1000;

    class Forge extends SpecializedFluidNeighborInfo.Forge implements IFluidUpdateHelper
    {
        public Forge(@Nonnull final IBlockAccess accessIn, @Nonnull final BlockPos originIn, @Nonnull final FluidState originStateIn, final int radius) {
            super(accessIn, originIn, originStateIn, radius);
        }
    }

    class Vanilla extends SpecializedFluidNeighborInfo.Vanilla implements IFluidUpdateHelper
    {
        public Vanilla(@Nonnull final IBlockAccess accessIn, @Nonnull final BlockPos originIn, @Nonnull final FluidState originStateIn, final int radius) {
            super(accessIn, originIn, originStateIn, radius);
        }
    }

    // ------------------------
    // index-relative functions
    // ------------------------

    default int calculateFlowCostI(final int xi, final int yi, final int zi, final int quantaPerBlock, final int flowMeta, final int flowCost, final int recurseDepth, @Nonnull final IntUnaryOperator downMeta, @Nonnull final EnumFacing checkedSide) {
        int cost = DEFAULT_COST;

        for(int adjSide = 0; adjSide < 4; adjSide++) {
            @Nonnull final EnumFacing side = EnumFacing.HORIZONTALS[adjSide];
            if(side != checkedSide) {
                final int xio = xi + side.getXOffset(), zio = zi + side.getZOffset();

                @Nonnull final FluidState fluid = getFluidStateI(xio, yi, zio);
                final int cappedLvl = Math.min(flowMeta + recurseDepth, quantaPerBlock - 1);

                if((!FluidloggedUtils.isCompatibleFluid(fluid, getOrigin()) || !fluid.isSource()) && canFlowIntoI(xi, yi, zi, cappedLvl, side, true, true)) {
                    if(canFlowIntoI(xio, yi, zio, downMeta.applyAsInt(cappedLvl), getOrigin().getDownDensityFace(), true, true)) return recurseDepth;
                    else if(recurseDepth < quantaPerBlock >> flowCost) cost = Math.min(cost, calculateFlowCostI(xio, yi, zio, quantaPerBlock, flowMeta, flowCost, recurseDepth + flowCost, downMeta, side.getOpposite()));
                }
            }
        }

        return cost;
    }

    @Nonnull
    default int[] getOptimalFlowDirectionsI(final int xi, final int yi, final int zi, final int quantaPerBlock, final int flowMeta, final int flowCost, @Nonnull final IntUnaryOperator downMeta) {
        @Nonnull final int[] adjFlowCost = new int[4];

        for(int sideI = 0; sideI < 4; sideI++) {
            adjFlowCost[sideI] = DEFAULT_COST;
            @Nonnull final EnumFacing side = EnumFacing.HORIZONTALS[sideI];
            final int xio = xi + side.getXOffset(), zio = zi + side.getZOffset();

            @Nonnull final FluidState fluid = getFluidStateI(xio, yi, zio);
            if((!FluidloggedUtils.isCompatibleFluid(fluid, getOrigin()) || !fluid.isSource()) && canFlowIntoI(xi, yi, zi, flowMeta, side, true, true)) {
                if(canFlowIntoI(xio, yi, zio, downMeta.applyAsInt(flowMeta), getOrigin().getDownDensityFace(), true, true)) adjFlowCost[sideI] = 0;
                else adjFlowCost[sideI] = flowMeta < quantaPerBlock ? calculateFlowCostI(xio, yi, zio, quantaPerBlock, flowMeta, flowCost, flowCost, downMeta, side.getOpposite()) : DEFAULT_COST;
            }
        }

        @Nonnull final int[] isOptimalFlowDirection = new int[4];
        final int min = Ints.min(adjFlowCost);
        for(int sideI = 0; sideI < 4; sideI++) isOptimalFlowDirection[sideI] = adjFlowCost[sideI] == min ? flowMeta : -1;

        // allow fluidlogged source blocks to be created while non-source fluidlogging is disabled
        if(!FluidloggedAPIConfig.nonSourceFluidlogging && FluidloggedUtils.canCreateSource(getOrigin().getState(), getCache().getWorld(), getPosIB(xi, yi, zi))) {
            for(int sideI = 0; sideI < 4; sideI++) if(min == DEFAULT_COST || isOptimalFlowDirection[sideI] == -1 && canFluidFlowI(xi, yi, zi, EnumFacing.HORIZONTALS[sideI])) {
                int adj = getOrigin().isSource() ? 1 : 0;

                @Nonnull final EnumFacing side = EnumFacing.HORIZONTALS[sideI];
                final int xio = xi + side.getXOffset(), zio = zi + side.getZOffset();

                // check that this position is fluidloggable with a source block, and that it matches the criteria to become a source
                if((getBlockStateI(xio, yi + getOrigin().getDensityDir(), zio).getMaterial().isSolid()
                || !canFluidFlowI(xio, yi, zio, getOrigin().getDownDensityFace()) || isSourceI(xio, yi + getOrigin().getDensityDir(), zio, getOrigin().getUpDensityFace()))
                && FluidloggedUtils.isStateFluidloggable(getBlockStateI(xio, yi, zio), getCache(), getPosIB(xio, yi, zio), getOrigin().toSource())) {

                    // check neighbors
                    for(@Nonnull final EnumFacing sideA : EnumFacing.HORIZONTALS) {
                        if(sideA != side.getOpposite() && canFluidFlowI(xio, yi, zio, sideA)) {
                            final int xioA = xio + sideA.getXOffset(), zioA = zio + sideA.getZOffset();

                            @Nonnull final FluidState fluidState = getFluidStateI(xioA, yi, zioA);
                            if(fluidState.isSource() && FluidloggedUtils.isCompatibleFluid(getOrigin(), fluidState) && canFluidFlowI(xioA, yi, zioA, sideA.getOpposite()) && ++adj == 2) {
                                isOptimalFlowDirection[sideI] = getOrigin().toSource().getLevel();
                                break;
                            }
                        }
                    }
                }
            }
        }

        return isOptimalFlowDirection;
    }

    default boolean isSourceI(final int xi, final int yi, final int zi, @Nonnull final EnumFacing side) {
        return isCompatibleFluidI(xi, yi, zi) && canFluidFlowI(xi, yi, zi, side) && getFluidStateI(xi, yi, zi).isSource();
    }

    default boolean flowIntoI(final int xi, final int yi, final int zi, final int level, @Nonnull final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching, final int blockFlags) {
        boolean ret = false;
        if(level >= 0 && canFlowIntoI(xi, yi, zi, level, sideToCheck, checkReplaceable, allowMatching)) {
            final int xio = xi + sideToCheck.getDirectionVec().getX(), yio = yi + sideToCheck.getDirectionVec().getY() *- getOrigin().getDensityDir(), zio = zi + sideToCheck.getDirectionVec().getZ();
            if(getOrigin().getBlock() instanceof BlockFluidFinite || !isCompatibleFluidI(xio, yio, zio)) { // don't flow into compatible fluids, they will update on their own to the proper level
                final boolean vaporize = vaporizeI(xio, yio, zio, getOrigin().withLevel(level), sideToCheck.getOpposite());
                // set local fire tick gamerule cache to false, as to not check isVaporizable twice
                @Nullable final Boolean prevFireTick = getDoFireTick();
                setDoFireTick(Boolean.FALSE);

                // if the block can be replaced by this fluid (can cannot be fluidlogged by it)
                if(vaporize || isReplaceableI(xio, yio, zio, getOrigin().withLevel(level), sideToCheck.getOpposite(), true, false)) {
                    if(!vaporize) {
                        @Nonnull final IBlockState oldState = getBlockStateI(xio, yio, zio);
                        if(oldState.getBlock() != getOrigin().getBlock() && getOrigin().getMaterial() == Material.LAVA && !oldState.getBlock().isAir(oldState, getCache(), getPosIB(xio, yio, zio)))
                            FluidloggedUtils.playVaporizeEffects(getCache().getWorld(), getCache().mutablePos, getOrigin().withLevel(level).createFluidStack());

                        else if(oldState.getMaterial().isToolNotRequired()) oldState.getBlock().dropBlockAsItem(getCache().getWorld(), getPosIB(xio, yio, zio), oldState, 0);
                    }

                    ret = getCache().getWorld().setBlockState(getPosIB(xio, yio, zio), getOrigin().withLevel(level).getState(), blockFlags);
                }

                // if the block can be fluidlogged with this fluid
                else if(canFluidFlowI(xio, yio, zio, sideToCheck.getOpposite()) && isFluidloggableI(xio, yio, zio, getOrigin().withLevel(level), sideToCheck.getOpposite(), true, false)) {
                    ret = FluidloggedUtils.setFluidState(getCache().getWorld(), getPosIB(xio, yio, zio), getBlockStateI(xio, yio, zio), getOrigin().withLevel(level), false, blockFlags);
                }

                // restore local fire tick gamerule cache
                setDoFireTick(prevFireTick);
            }
        }

        return ret;
    }

    default void setFluidI(final int xi, final int yi, final int zi, @Nonnull final FluidState fluidToPlace, final boolean setToAir, final int tickRate, final int blockFlags) {
        // set IBlockState
        if(getBlockStateI(xi, yi, zi).getBlock() == getOrigin().getBlock() || !setToAir && vaporizeI(xi, yi, zi, fluidToPlace, null)) {
            if(setToAir) getCache().getWorld().setBlockState(getPosIB(xi, yi, zi), BlockStateContainer.AIR_BLOCK_STATE);
            else {
                getCache().getWorld().setBlockState(getPosIB(xi, yi, zi), fluidToPlace.getState(), blockFlags);
                if(tickRate > 0) {
                    getCache().getWorld().scheduleUpdate(getCache().mutablePos, getOrigin().getBlock(), tickRate);
                    getCache().getWorld().notifyNeighborsOfStateChange(getCache().mutablePos, getOrigin().getBlock(), false);
                }
            }
        }
        // set FluidState
        else {
            if(setToAir) FluidloggedUtils.setFluidState(getCache().getWorld(), getPosIB(xi, yi, zi), getBlockStateI(xi, yi, zi), FluidState.EMPTY, false);
            else {
                if(!isFluidloggableI(xi, yi, zi, fluidToPlace, null, false, false))
                    FluidloggedUtils.setFluidState(getCache().getWorld(), getPosIB(xi, yi, zi), getBlockStateI(xi, yi, zi), FluidState.EMPTY, false);
                else {
                    FluidloggedUtils.setFluidState(getCache().getWorld(), getPosIB(xi, yi, zi), getBlockStateI(xi, yi, zi), fluidToPlace, false, blockFlags);
                    if(tickRate > 0) {
                        getCache().getWorld().scheduleUpdate(getCache().mutablePos, getOrigin().getBlock(), tickRate);
                        getCache().getWorld().notifyNeighborsOfStateChange(getCache().mutablePos, getOrigin().getBlock(), false);
                    }
                }
            }
        }
    }

    default void resetDataAtI(final int xi, final int yi, final int zi) {
        // @Nonnull final IBlockState oldState = getBlockStateI(xi, yi, zi);
        /*final int index = getCache().getIndexI(xi, yi, zi);

        getCache().fluids.remove(index);
        getCache().states.remove(index);
        getCache().tiles.remove(index);

        for(int i = 0; i < 16; i++) {
            getIsFluidloggable()[i * (getIsFluidloggable().length >> 4) + index] = null;
            getIsReplaceable()[i * (getIsReplaceable().length >> 4) + index] = null;
        }*/

        /*if(oldState != getBlockStateI(xi, yi, zi)) {
            for(int xo = Math.max(0, xi - 1); xo < Math.min(getCanFluidFlow().length, xi + 2); xo++) {
                for(int yo = Math.max(0, yi - 1); yo < Math.min(getCanFluidFlow().length, yi + 2); yo++) {
                    for(int zo = Math.max(0, zi - 1); zo < Math.min(getCanFluidFlow().length, zi + 2); zo++) {
                        for(@Nonnull final EnumFacing side : EnumFacing.VALUES) getCanFluidFlow()[side.getIndex() * getIsCompatibleFluid().length + index] = null;
                    }
                }
            }
        }*/
    }

    default boolean vaporizeI(final int xi, final int yi, final int zi, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck) {
        if(isVaporizableI(xi, yi, zi, fluidToPlace, sideToCheck)) {
            FluidloggedUtils.playVaporizeEffects(getCache().getWorld(), getPosIB(xi, yi, zi), fluidToPlace.createFluidStack());
            return true;
        }

        return false;
    }

    // -------------------------
    // origin-relative functions
    // -------------------------

    default int calculateFlowCost(final int x, final int y, final int z, final int quantaPerBlock, final int flowMeta, final int flowCost, final int recurseDepth, @Nonnull final IntUnaryOperator downMeta, @Nonnull final EnumFacing checkedSide) {
        return calculateFlowCostI(getXI(x), getYI(y), getZI(z), quantaPerBlock, flowMeta, flowCost, recurseDepth, downMeta, checkedSide);
    }

    @Nonnull
    default int[] getOptimalFlowDirections(final int x, final int y, final int z, final int quantaPerBlock, final int flowMeta, final int flowCost, @Nonnull final IntUnaryOperator downMeta) {
        return getOptimalFlowDirectionsI(getXI(x), getYI(y), getZI(z), quantaPerBlock, flowMeta, flowCost, downMeta);
    }

    default boolean isSource(final int x, final int y, final int z, @Nonnull final EnumFacing side) {
        return isSourceI(getXI(x), getYI(y), getZI(z), side);
    }

    default boolean flowInto(final int x, final int y, final int z, final int level, @Nonnull final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching, final int blockFlags) {
        return flowIntoI(getXI(x), getYI(y), getZI(z), level, sideToCheck, checkReplaceable, allowMatching, blockFlags);
    }

    default void setFluid(final int x, final int y, final int z, @Nonnull final FluidState fluidToPlace, final boolean setToAir, final int tickRate) {
        setFluidI(getXI(x), getYI(y), getZI(z), fluidToPlace, setToAir, tickRate, Constants.BlockFlags.SEND_TO_CLIENTS);
    }

    default void setFluid(final int x, final int y, final int z, @Nonnull final FluidState fluidToPlace, final boolean setToAir, final int tickRate, final int blockFlags) {
        setFluidI(getXI(x), getYI(y), getZI(z), fluidToPlace, setToAir, tickRate, blockFlags);
    }

    default void resetDataAt(final int x, final int y, final int z) {
        resetDataAtI(getXI(x), getYI(y), getZI(z));
    }

    default boolean vaporize(final int x, final int y, final int z, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck) {
        return vaporizeI(getXI(x), getYI(y), getZI(z), fluidToPlace, sideToCheck);
    }
}
