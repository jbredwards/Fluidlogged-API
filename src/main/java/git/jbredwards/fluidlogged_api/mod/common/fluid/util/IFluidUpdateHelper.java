/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.fluid.util;

import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.impl.SpecializedFluidNeighborInfo;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;
import it.unimi.dsi.fastutil.ints.IntSets;
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

    default int calculateFlowCostI(final int xi, final int yi, final int zi, final int quantaPerBlock, final int flowMeta, final int flowCost, final int recurseDepth, @Nonnull final IntUnaryOperator downMeta, @Nonnull final IntSet checkedX, @Nonnull final IntSet checkedZ) {
        @Nonnull final IntSet adjX = new IntOpenHashSet(checkedX), adjZ = new IntOpenHashSet(checkedZ);
        adjX.add(xi);
        adjZ.add(zi);
        int cost = 1000;

        for(int adjSide = 0; adjSide < 4; adjSide++) {
            @Nonnull final EnumFacing side = EnumFacing.HORIZONTALS[adjSide];

            final int xio = xi + side.getXOffset(), zio = zi + side.getZOffset();
            if(checkedX.contains(xio) && checkedZ.contains(zio)) continue;

            @Nonnull final FluidState fluid = getFluidStateI(xio, yi, zio);
            final int cappedLvl = Math.max(flowMeta + recurseDepth, quantaPerBlock);

            if((!FluidloggedUtils.isCompatibleFluid(fluid, getOrigin()) || !fluid.isSource()) && canFlowIntoI(xi, yi, zi, cappedLvl, side, true, true)) {
                if(canFlowIntoI(xio, yi, zio, downMeta.applyAsInt(cappedLvl), getOrigin().getDownDensityFace(), true, true)) return recurseDepth;
                else if(/*flowMeta +*/ recurseDepth + flowCost < quantaPerBlock - flowCost) cost = Math.min(cost, calculateFlowCostI(xio, yi, zio, quantaPerBlock, flowMeta, flowCost, recurseDepth + flowCost, downMeta, adjX, adjZ));
            }
        }

        return cost;
    }

    @Nonnull
    default boolean[] getOptimalFlowDirectionsI(final int xi, final int yi, final int zi, final int quantaPerBlock, final int flowMeta, final int flowCost, @Nonnull final IntUnaryOperator downMeta) {
        @Nonnull final IntSet checkedX = IntSets.singleton(xi), checkedZ = IntSets.singleton(zi);
        @Nonnull final int[] adjFlowCost = new int[4];

        for(int sideI = 0; sideI < 4; sideI++) {
            adjFlowCost[sideI] = 1000;
            @Nonnull final EnumFacing side = EnumFacing.HORIZONTALS[sideI];
            final int xio = xi + side.getXOffset(), zio = zi + side.getZOffset();

            @Nonnull final FluidState fluid = getFluidStateI(xio, yi, zio);
            if((!FluidloggedUtils.isCompatibleFluid(fluid, getOrigin()) || !fluid.isSource()) && canFlowIntoI(xi, yi, zi, flowMeta, side, true, true)) {
                /*if(canFluidFlowI(xio, yi, zio, side.getOpposite()) && isCompatibleFluidI(xio, yi, zio)) {
                    @Nonnull final FluidState fluidState = getFluidStateI(xio, yi, zio);
                    if(!fluidState.isEmpty() && fluidState.getLevel() >= flowMeta) {
                        adjFlowCost[sideI] = 0;
                        continue;
                    }
                }*/

                if(canFlowIntoI(xio, yi, zio, downMeta.applyAsInt(flowMeta), getOrigin().getDownDensityFace(), true, true)) adjFlowCost[sideI] = 0;
                else adjFlowCost[sideI] = flowMeta < quantaPerBlock ? calculateFlowCostI(xio, yi, zio, quantaPerBlock, flowMeta, flowCost, flowCost, downMeta, checkedX, checkedZ) : 1000;
            }
        }

        @Nonnull final boolean[] isOptimalFlowDirection = new boolean[4];
        final int min = Ints.min(adjFlowCost);

        for(int sideI = 0; sideI < 4; sideI++) isOptimalFlowDirection[sideI] = adjFlowCost[sideI] == min;
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

                        else if(oldState.getBlock().getHarvestTool(oldState) == null) oldState.getBlock().dropBlockAsItem(getCache().getWorld(), getPosIB(xio, yio, zio), oldState, 0);
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

    default void setFluidI(final int xi, final int yi, final int zi, @Nonnull final FluidState fluidToPlace, final boolean setToAir, final int tickRate) {
        // set IBlockState
        if(getBlockStateI(xi, yi, zi) == getOrigin().getState() || !setToAir && vaporizeI(xi, yi, zi, fluidToPlace, null)) {
            if(setToAir) getCache().getWorld().setBlockState(getPosIB(xi, yi, zi), BlockStateContainer.AIR_BLOCK_STATE);
            else {
                getCache().getWorld().setBlockState(getPosIB(xi, yi, zi), fluidToPlace.getState(), Constants.BlockFlags.SEND_TO_CLIENTS);
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
                    FluidloggedUtils.setFluidState(getCache().getWorld(), getPosIB(xi, yi, zi), getBlockStateI(xi, yi, zi), fluidToPlace, false, Constants.BlockFlags.SEND_TO_CLIENTS);
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

    default int calculateFlowCost(final int x, final int y, final int z, final int quantaPerBlock, final int flowMeta, final int flowCost, final int recurseDepth, @Nonnull final IntUnaryOperator downMeta, @Nonnull final IntSet checkedX, @Nonnull final IntSet checkedZ) {
        return calculateFlowCostI(getXI(x), getYI(y), getZI(z), quantaPerBlock, flowMeta, flowCost, recurseDepth, downMeta, checkedX, checkedZ);
    }

    @Nonnull
    default boolean[] getOptimalFlowDirections(final int x, final int y, final int z, final int quantaPerBlock, final int flowMeta, final int flowCost, @Nonnull final IntUnaryOperator downMeta) {
        return getOptimalFlowDirectionsI(getXI(x), getYI(y), getZI(z), quantaPerBlock, flowMeta, flowCost, downMeta);
    }

    default boolean isSource(final int x, final int y, final int z, @Nonnull final EnumFacing side) {
        return isSourceI(getXI(x), getYI(y), getZI(z), side);
    }

    default boolean flowInto(final int x, final int y, final int z, final int level, @Nonnull final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching, final int blockFlags) {
        return flowIntoI(getXI(x), getYI(y), getZI(z), level, sideToCheck, checkReplaceable, allowMatching, blockFlags);
    }

    default void setFluid(final int x, final int y, final int z, @Nonnull final FluidState fluidToPlace, final boolean setToAir, final int tickRate) {
        setFluidI(getXI(x), getYI(y), getZI(z), fluidToPlace, setToAir, tickRate);
    }

    default void resetDataAt(final int x, final int y, final int z) {
        resetDataAtI(getXI(x), getYI(y), getZI(z));
    }

    default boolean vaporize(final int x, final int y, final int z, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck) {
        return vaporizeI(getXI(x), getYI(y), getZI(z), fluidToPlace, sideToCheck);
    }
}
