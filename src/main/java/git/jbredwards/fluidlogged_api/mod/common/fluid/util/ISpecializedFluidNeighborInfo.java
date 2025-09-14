/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.fluid.util;

import git.jbredwards.fluidlogged_api.mod.asm.iface.IConditionalFluid;
import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.block.BlockDoor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public interface ISpecializedFluidNeighborInfo extends IFluidNeighborInfo
{
    interface Forge extends ISpecializedFluidNeighborInfo
    {
        @Override
        default boolean canDisplaceI(final int xi, final int yi, final int zi) {
            @Nonnull final IBlockState state = getBlockStateI(xi, yi, zi);
            if(FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidFromState(state), getOrigin().getFluid())) return true;

            @Nullable final Boolean displacement = getOrigin().getDisplacements().get(state.getBlock());
            if(displacement != null) return displacement;

            @Nonnull final Material material = state.getMaterial();
            return !material.blocksMovement() && material != Material.PORTAL && material != Material.STRUCTURE_VOID;
        }

        @Override
        default int getEffectiveQuantaI(final int xi, final int yi, final int zi) {
            @Nonnull final FluidState fluidState = getFluidStateI(xi, yi, zi);
            if(!FluidloggedUtils.isCompatibleFluid(fluidState, getOrigin())) return getCache().isAirBlock(getPosIB(xi, yi, zi)) ? 0 : -1;

            final int quantaValue = fluidState.getQuantaValue();
            return quantaValue > 0 && quantaValue < getOrigin().getQuantaPerBlock() && hasVerticalFlowI(xi, yi, zi) ? getOrigin().getQuantaPerBlock() : quantaValue;
        }
    }

    interface Vanilla extends ISpecializedFluidNeighborInfo
    {
        @Override
        default boolean canDisplaceI(final int xi, final int yi, final int zi) {
            @Nonnull final IBlockState state = getBlockStateI(xi, yi, zi);
            if(FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidFromState(state), getOrigin().getFluid())) return true;

            else if(!(state.getBlock() instanceof BlockDoor) && state.getBlock() != Blocks.STANDING_SIGN && state.getBlock() != Blocks.LADDER && state.getBlock() != Blocks.REEDS)
                return !state.getMaterial().blocksMovement() && state.getMaterial() != Material.PORTAL && state.getMaterial() != Material.STRUCTURE_VOID;

            else return true;
        }

        @Override
        default int getEffectiveQuantaI(final int xi, final int yi, final int zi) {
            @Nonnull final FluidState fluidState = getFluidStateI(xi, yi, zi);
            if(!FluidloggedUtils.isCompatibleFluid(fluidState, getOrigin())) return getCache().isAirBlock(getPosIB(xi, yi, zi)) ? 0 : -1;

            final int quantaValue = fluidState.getLevel();
            return quantaValue >= 8 ? 8 : 8 - quantaValue;
        }
    }

    // ----------------
    // properties cache
    // ----------------

    @Nullable
    Boolean getDoFireTick();

    @Nonnull // return value matches the parameter
    Boolean setDoFireTick(@Nullable final Boolean doFireTickIn);

    /*@Nonnull
    Int2BooleanMap getIsFluidloggable(); //Boolean[] getIsFluidloggable();

    @Nonnull
    Int2BooleanMap getIsReplaceable(); //Boolean[] getIsReplaceable();*/

    // ------------------------
    // index-relative functions
    // ------------------------

    boolean canDisplaceI(final int xi, final int yi, final int zi);
    default boolean canFlowIntoI(final int xi, final int yi, final int zi, final int flowMeta, @Nonnull final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching) {
        final int xio = xi + sideToCheck.getDirectionVec().getX(), yio = yi + sideToCheck.getDirectionVec().getY() *- getOrigin().getDensityDir(), zio = zi + sideToCheck.getDirectionVec().getZ();
        if(getOrigin().getBlock() instanceof IConditionalFluid) {
            if(((IConditionalFluid)getOrigin().getBlock()).cannotFlowAt(getCache(), getPosIB(xio, yio, zio), getOrigin().withLevel(flowMeta))) return false;
        }

        return canFluidFlowI(xi, yi, zi, sideToCheck) && (isReplaceableI(xio, yio, zio, getOrigin().withLevel(flowMeta), sideToCheck.getOpposite(), checkReplaceable, allowMatching)
                || canFluidFlowI(xio, yio, zio, sideToCheck.getOpposite()) && isFluidloggableI(xio, yio, zio, getOrigin().withLevel(flowMeta), sideToCheck.getOpposite(), checkReplaceable, allowMatching));
    }

    int getEffectiveQuantaI(final int xi, final int yi, final int zi);
    default boolean hasVerticalFlowI(final int xi, final int yi, final int zi) {
        return isCompatibleFluidI(xi, yi + 1, zi) && canFluidFlowI(xi, yi, zi, getOrigin().getUpDensityFace()) && canFluidFlowI(xi, yi + 1, zi, getOrigin().getDownDensityFace());
    }

    default boolean isFluidloggableI(final int xi, final int yi, final int zi, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching) {
        // check if the fluid state here (if present) can be replaced by the new fluid state
        if(checkReplaceable) {
            @Nonnull final FluidState existing = getFluidStateI(xi, yi, zi);
            if(existing.getBlock() instanceof IFluidloggableFluid && !((IFluidloggableFluid)existing.getBlock()).isReplaceableByOther(getCache().getWorld(), existing, fluidToPlace, allowMatching)) return false;
        }

        /*final int index = fluidToPlace.getMetadata() * getCache().diamXZ * getCache().diamY * getCache().diamXZ + getCache().getIndexI(xi, yi, zi);
        if(getIsFluidloggable()[index] != null) return getIsFluidloggable()[index];
        else*/ if(isVaporizableI(xi, yi, zi, fluidToPlace, sideToCheck)) return true; // getIsFluidloggable()[index] = false;

        // default
        else return /*getIsFluidloggable()[index] =*/ FluidloggedUtils.isStateFluidloggable(getBlockStateI(xi, yi, zi), getCache(), getPosIB(xi, yi, zi), fluidToPlace);
    }

    default boolean isReplaceableI(final int xi, final int yi, final int zi, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching) {
        // check if the fluid state here (if present) can be replaced by the new fluid state
        if(checkReplaceable) {
            @Nonnull final FluidState existing = getFluidStateI(xi, yi, zi);
            if(existing.getBlock() instanceof IFluidloggableFluid && !((IFluidloggableFluid)existing.getBlock()).isReplaceableByOther(getCache().getWorld(), existing, fluidToPlace, allowMatching)) return false;
        }

        /*final int index = fluidToPlace.getMetadata() * getCache().diamXZ * getCache().diamY * getCache().diamXZ + getCache().getIndexI(xi, yi, zi);
        if(getIsReplaceable()[index] != null) return getIsReplaceable()[index];
        else*/ if(isVaporizableI(xi, yi, zi, fluidToPlace, sideToCheck)) return true; // return getIsReplaceable()[index] = true;

        // default
        else return /*getIsReplaceable()[index] =*/ getCache().isAirBlock(getPosIB(xi, yi, zi)) || canDisplaceI(xi, yi, zi) && !isFluidloggableI(xi, yi, zi, fluidToPlace.toSource(), sideToCheck, false, false);
    }

    default boolean isVaporizableI(final int xi, final int yi, final int zi, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck) {
        if(FluidloggedAPIConfig.lavalogVaporizeFlammable != FluidloggedAPIConfig.LavaVaporizingMode.NEVER
        && fluidToPlace.getMaterial() == Material.LAVA && !getCache().getWorld().isOutsideBuildHeight(getPosIB(xi, yi, zi))
        && (getDoFireTick() == null ? setDoFireTick(getCache().getWorld().getGameRules().getBoolean("doFireTick")) : getDoFireTick())) {
            /*if(FluidloggedAPIConfig.lavalogVaporizeFlammable == FluidloggedAPIConfig.LavaVaporizingMode.ALWAYS) {
                @Nonnull final IBlockState state = getBlockStateI(xi, yi, zi);
                return !FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidFromState(state), getOrigin().getFluid())
                        && !state.getBlock().isAir(state, getCache(), getPosIB(xi, yi, zi))
                        && state.getBlockHardness(getCache().getWorld(), getPosIB(xi, yi, zi)) > -1;
            }

            else*/ if(getBlockStateI(xi, yi, zi).getMaterial().getCanBurn()) {
                if(isCompatibleFluidI(xi, yi, zi)) return true; // block here is fluidlogged with this fluid, and is flammable
                else if(getFluidStateI(xi, yi, zi).isEmpty()) return FluidloggedAPIConfig.lavalogVaporizeFlammable == FluidloggedAPIConfig.LavaVaporizingMode.FLAMMABLE;
                        // || (sideToCheck == null || canFluidFlowI(xi, yi, zi, sideToCheck)) && FluidloggedUtils.isStateFluidloggable(getBlockStateI(xi, yi, zi), getCache(), getPosIB(xi, yi, zi), fluidToPlace);
            }
        }

        return false;
    }

    // -------------------------
    // origin-relative functions
    // -------------------------

    default boolean canDisplace(final int x, final int y, final int z) {
        return canDisplaceI(getXI(x), getYI(y), getZI(z));
    }

    default boolean canFlowInto(final int x, final int y, final int z, final int flowMeta, @Nonnull final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching) {
        return canFlowIntoI(getXI(x), getYI(y), getZI(z), flowMeta, sideToCheck, checkReplaceable, allowMatching);
    }

    default int getEffectiveQuanta(final int x, final int y, final int z) {
        return getEffectiveQuantaI(getXI(x), getYI(y), getZI(z));
    }

    default boolean hasVerticalFlow(final int x, final int y, final int z) {
        return hasVerticalFlowI(getXI(x), getYI(y), getZI(z));
    }

    default boolean isFluidloggable(final int x, final int y, final int z, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching) {
        return isFluidloggableI(getXI(x), getYI(y), getZI(z), fluidToPlace, sideToCheck, checkReplaceable, allowMatching);
    }

    default boolean isReplaceable(final int x, final int y, final int z, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck, final boolean checkReplaceable, final boolean allowMatching) {
        return isReplaceableI(getXI(x), getYI(y), getZI(z), fluidToPlace, sideToCheck, checkReplaceable, allowMatching);
    }

    default boolean isVaporizable(final int x, final int y, final int z, @Nonnull final FluidState fluidToPlace, @Nullable final EnumFacing sideToCheck) {
        return isVaporizableI(getXI(x), getYI(y), getZI(z), fluidToPlace, sideToCheck);
    }
}
