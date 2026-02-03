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

package git.jbredwards.fluidlogged_api.mod.common.fluid.handler;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConditionalFluid;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.IFluidUpdateHelper;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.ISpecializedFluidNeighborInfo;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.BlockFluidBase;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Implemented by
 * {@link net.minecraft.block.BlockDynamicLiquid BlockDynamicLiquid}, {@link BlockLiquid},
 * {@link net.minecraftforge.fluids.BlockFluidBase BlockFluidBase},
 * {@link net.minecraftforge.fluids.BlockFluidClassic BlockFluidClassic}, and
 * {@link net.minecraftforge.fluids.BlockFluidFinite BlockFluidFinite} at runtime.
 * @author jbred
 *
 */
public final class FluidFlowHandler
{
    // ===========
    // FLOW VECTOR
    // ===========

    public static double getFlowAngle(@Nonnull final Vec3d flowVec) {
        return flowVec.x == 0 && flowVec.z == 0 ? -1000 : MathHelper.atan2(flowVec.z, flowVec.x) - Math.PI / 2;
    }

    public static double getFlowAngle(@Nonnull final ISpecializedFluidNeighborInfo info) {
        return getFlowAngle(getFlowVec(info));
    }

    @Nonnull
    public static Vec3d getFlowVec(@Nonnull final ISpecializedFluidNeighborInfo info) {
        // info.getCache().getWorld().profiler.startSection("fluidFlowVec");

        final int max = info.getOrigin().getQuantaPerBlock();
        final int decay = max - info.getEffectiveQuanta(0, 0, 0);

        @Nonnull final EnumFacing down = info.getOrigin().getDownDensityFace();
        @Nonnull final EnumFacing up = info.getOrigin().getUpDensityFace();

        @Nonnull Vec3d vec = Vec3d.ZERO;
        for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
            if(info.canFluidConnect(0, 0, 0, side)) {
                final int xo = side.getXOffset(), zo = side.getZOffset();
                if(!info.isCompatibleFluid(xo, 0, zo) || info.canFluidConnect(xo, 0, zo, side.getOpposite())) {
                    int otherDecay = max - info.getEffectiveQuanta(xo, 0, zo);
                    if(otherDecay >= max) {
                        if(info.canFluidConnect(xo, 0, zo, down) && (!info.isCompatibleFluid(xo, -1, zo) || info.canFluidFlow(xo, -1, zo, up))) {
                            otherDecay = max - info.getEffectiveQuanta(xo, -1, zo);
                            if(otherDecay < max) {
                                final int power = otherDecay - (decay - max);
                                vec = vec.add(xo * power, 0, zo * power);
                            }
                        }
                    }

                    else {
                        final int power = otherDecay - decay;
                        vec = vec.add(xo * power, 0, zo * power);
                    }
                }
            }
        }

        // info.getCache().getWorld().profiler.endSection();
        return vec.normalize();
    }

    // =================
    // BlockFluidClassic
    // =================

    public static void updateClassic(@Nonnull final World world, @Nonnull final BlockPos origin, @Nonnull final FluidState originState) {
        final int flowCost = originState.getFlowCost(world);
        final int slopeDist = originState.getQuantaPerBlock() >> flowCost;
        if(world.isRemote || !world.isAreaLoaded(origin, slopeDist) || !originState.isValid()) return;

        // world.profiler.startSection("fluidUpdateClassic");
        @Nonnull final IFluidUpdateHelper helper = new IFluidUpdateHelper.Forge(world, origin, originState, slopeDist);

        // check adjacent block levels if non-source
        int quantaRemaining = originState.getQuantaPerBlock() - originState.getLevel();
        if(quantaRemaining < originState.getQuantaPerBlock()) {
            final int expQuanta;

            int adjacentSourceBlocks = 0;
            if(originState.getQuantaPerBlock() > 0 && FluidloggedUtils.canCreateSource(originState.getState(), world, origin) && (helper.getBlockState(0, -1, 0).getMaterial().isSolid()
            || !helper.canFluidFlow(0, 0, 0, originState.getDownDensityFace()) || helper.isSource(0, -1, 0, originState.getUpDensityFace()))) {
                for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
                    if(helper.canFluidFlow(0, 0, 0, side) && helper.isSource(side.getXOffset(), 0, side.getZOffset(), side.getOpposite()))
                        adjacentSourceBlocks++;
                }
            }

            // new source block
            if(adjacentSourceBlocks >= 2
            && (helper.getBlockState(0, -1, 0).getMaterial().isSolid()
            || !helper.canFluidFlow(0, 0, 0, originState.getDownDensityFace())
            || helper.isSource(0, -1, 0, originState.getUpDensityFace())))
                expQuanta = originState.getQuantaPerBlock();

            // vertical flow into block
            else if(helper.hasVerticalFlow(0, 0, 0))
                expQuanta = originState.getQuantaPerBlock() - flowCost;

            // use the largest neighbor level
            else {
                int maxQuanta = -100;
                if(originState.getQuantaPerBlock() > 0) for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
                    if(helper.canFluidFlow(0, 0, 0, side) && helper.canFluidFlow(side.getXOffset(), 0, side.getZOffset(), side.getOpposite()))
                        maxQuanta = Math.max(helper.getEffectiveQuanta(side.getXOffset(), 0, side.getZOffset()), maxQuanta);
                }

                expQuanta = maxQuanta - flowCost;
            }

            // decay calculation
            if(expQuanta != quantaRemaining) {
                quantaRemaining = expQuanta;
                helper.setFluid(0, 0, 0, expQuanta <= 0 ? FluidState.EMPTY : originState.withLevel(originState.getQuantaPerBlock() - expQuanta), expQuanta <= 0, originState.getBlock().tickRate(world));
                // changed data in world, reset non-chunk data in helper
                helper.resetDataAt(0, 0, 0);
            }
        }

        if(helper.getFluidState(0, 0, 0).isEmpty()) {
            // world.profiler.endSection();
            return;
        }

        // flow vertically if possible
        if(helper.canFlowInto(0, 0, 0, flowCost, originState.getDownDensityFace(), true, false) && (!helper.getFluidState(0, 0, 0).isSource() || !helper.isCompatibleFluid(0, -1, 0))) {
            helper.flowInto(0, 0, 0, flowCost, originState.getDownDensityFace(), true, false, Constants.BlockFlags.DEFAULT);
            // world.profiler.endSection();
            return;
        }

        // flow outward if possible
        int flowMeta = originState.getQuantaPerBlock() - quantaRemaining + flowCost;
        if(flowMeta >= originState.getQuantaPerBlock()) {
            // world.profiler.endSection();
            return;
        }

        if(flowMeta >= 0 && (helper.getFluidState(0, 0, 0).isSource() || !helper.canFlowInto(0, 0, 0, flowCost, originState.getDownDensityFace(), true, true))) {
            if(helper.hasVerticalFlow(0, 0, 0)) flowMeta = flowCost;

            @Nonnull final int[] flowTo = helper.getOptimalFlowDirections(0, 0, 0, originState.getQuantaPerBlock(), flowMeta, flowCost, levelIn -> flowCost);
            for(int i = 0; i < 4; i++) if(flowTo[i] > -1) {
                @Nonnull final EnumFacing side = EnumFacing.HORIZONTALS[i];
                helper.flowInto(0, 0, 0, flowTo[i], side, true, false, Constants.BlockFlags.DEFAULT);
            }
        }

        // world.profiler.endSection();
    }

    // ==================
    // BlockDynamicLiquid
    // ==================

    public static void updateDynamic(@Nonnull final World world, @Nonnull final BlockPos origin, @Nonnull final FluidState originState, @Nonnull final Random rand) {
        final int flowCost = originState.getFlowCost(world);
        final int slopeDist = originState.getQuantaPerBlock() >> flowCost;
        if(world.isRemote || !world.isAreaLoaded(origin, slopeDist)) return;
        // world.profiler.startSection("fluidUpdateVanilla");

        @Nonnull final IFluidUpdateHelper helper = new IFluidUpdateHelper.Vanilla(world, origin, originState, slopeDist);
        int tickRate = originState.getBlock().tickRate(world);
        int level = originState.getLevel();

        boolean placeStatic = true;

        // check adjacent block levels if non-source
        if(level > 0) {
            int currentMinLevel = -100;
            int adjacentSourceBlocks = 0;
            for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
                if(!helper.canFluidFlow(0, 0, 0, side)) continue;
                final int xo = side.getXOffset(), zo = side.getZOffset();
                if(helper.isCompatibleFluid(xo, 0, zo) && helper.canFluidFlow(xo, 0, zo, side.getOpposite())) {
                    int neighborLevel = helper.getFluidState(xo, 0, zo).getLevel();

                    if(neighborLevel == 0) adjacentSourceBlocks++;
                    else if(neighborLevel >= 8) neighborLevel = 0;

                    currentMinLevel = currentMinLevel >= 0 && neighborLevel > currentMinLevel ? currentMinLevel : neighborLevel;
                }
            }

            int newLevel = currentMinLevel + flowCost;
            if(newLevel >= 8 || currentMinLevel < 0) newLevel = -1;

            // check for fluid above this
            if(helper.hasVerticalFlow(0, 0, 0)) {
                final int upLevel = helper.getFluidState(0, 1, 0).getLevel();
                if(upLevel >= 8) newLevel = upLevel;
                else newLevel = upLevel + 8;
            }

            // new source block
            if(adjacentSourceBlocks >= 2 && FluidloggedUtils.canCreateSource(originState.getState(), world, origin) && (helper.getBlockState(0, -1, 0).getMaterial().isSolid()
            || !helper.canFluidFlow(0, 0, 0, originState.getDownDensityFace()) || helper.isSource(0, -1, 0, originState.getUpDensityFace()))) {
                newLevel = 0;
            }

            // randomize lava update ticks (vanilla feature)
            if(originState.getMaterial() == Material.LAVA && newLevel < 8 && newLevel > level && rand.nextInt(4) != 0) tickRate <<= 2;

            // decay calculation
            if(level != newLevel) {
                level = newLevel;
                placeStatic = false;
                helper.setFluid(0, 0, 0, level < 0 ? FluidState.EMPTY : originState.withLevel(level), level < 0, tickRate);
            }
        }

        // place static block
        if(placeStatic) {
            @Nonnull final FluidState fluidState = originState.toStatic();
            if(helper.getBlockState(0, 0, 0) == originState.getState() || helper.vaporize(0, 0, 0, fluidState, null)) world.setBlockState(origin, fluidState.getState(), Constants.BlockFlags.SEND_TO_CLIENTS | Constants.BlockFlags.NO_RERENDER | Constants.BlockFlags.NO_OBSERVERS);
            else FluidloggedUtils.setFluidState(world, origin, helper.getBlockState(0, 0, 0), fluidState, false, Constants.BlockFlags.SEND_TO_CLIENTS | Constants.BlockFlags.NO_RERENDER | Constants.BlockFlags.NO_OBSERVERS);
        }

        // changed data in world, reset non-chunk data in helper
        helper.resetDataAt(0, 0, 0);
        if(helper.getFluidState(0, 0, 0).isEmpty()) {
            // world.profiler.endSection();
            return;
        }

        // mix with below if possible
        if(!FluidloggedAPIConfig.fixBadFluidMixing || helper.canFluidFlow(0, 0, 0, originState.getDownDensityFace())) {
            if(originState.getMaterial() == Material.LAVA && helper.getBlockState(0, -1, 0).getMaterial() == Material.WATER) {
                world.setBlockState(helper.getCache().offset(origin, originState.getDownDensityFace().getDirectionVec()), ForgeEventFactory.fireFluidPlaceBlockEvent(world, helper.getCache().mutablePos, origin, Blocks.STONE.getDefaultState()));
                ((BlockLiquid)originState.getBlock()).triggerMixEffects(world, helper.getCache().mutablePos);
                // world.profiler.endSection();
                return;
            }
        }

        // flow vertically if possible
        if(helper.canFlowInto(0, 0, 0, level >= 8 ? level : level + 8, originState.getDownDensityFace(), true, false) && (!helper.getFluidState(0, 0, 0).isSource() || !helper.isCompatibleFluid(0, -1, 0))) {
            helper.flowInto(0, 0, 0, level >= 8 ? level : level + 8, originState.getDownDensityFace(), true, false, Constants.BlockFlags.DEFAULT);
            // world.profiler.endSection();
            return;
        }

        // flow outward if possible
        if(level >= 0 && (helper.getFluidState(0, 0, 0).isSource() || !helper.canFlowInto(0, 0, 0, level >= 8 ? level : level + 8, originState.getDownDensityFace(), true, true))) {
            final int newLevel = level >= 8 ? flowCost : level + flowCost;
            if(newLevel < 8) {
                @Nonnull final int[] flowTo = helper.getOptimalFlowDirections(0, 0, 0, 8, newLevel, flowCost, levelIn -> levelIn >= 8 ? levelIn : levelIn + 8);
                for(int i = 0; i < 4; i++) if(flowTo[i] > -1) {
                    @Nonnull final EnumFacing side = EnumFacing.HORIZONTALS[i];
                    helper.flowInto(0, 0, 0, flowTo[i], side, true, false, Constants.BlockFlags.DEFAULT);
                }
            }
        }

        // world.profiler.endSection();
    }

    // ================
    // BlockFluidFinite
    // ================

    public static void updateFinite(@Nonnull final World world, @Nonnull final BlockPos origin, @Nonnull final FluidState originState, @Nonnull final Random rand) {
        final int tickRate = originState.getBlock().tickRate(world);

        boolean changed = false;
        int quantaRemaining = originState.getQuantaValue();
        @Nonnull final IFluidUpdateHelper helper = new IFluidUpdateHelper.Forge(world, origin, originState, 1);

        if(helper.canFluidFlow(0, 0, 0, originState.getDownDensityFace())) {
            @Nonnull final BlockPos below = origin.up(originState.getDensityDir());

            // flow vertically into void
            if(world.isOutsideBuildHeight(below)) {
                FluidloggedUtils.setFluidToAir(world, origin, null, Constants.BlockFlags.DEFAULT);
                return;
            }

            // merge vertically if possible
            final int prevRemaining = quantaRemaining;
            int amt = originState.getQuantaValueBelow(helper.getCache(), below, originState.getQuantaPerBlock());
            if(amt >= 0) {
                amt += quantaRemaining;
                if(amt > originState.getQuantaPerBlock()) {
                    if(helper.flowInto(0, 0, 0, originState.getQuantaPerBlock() - 1, originState.getDownDensityFace(), false, true, Constants.BlockFlags.DEFAULT)) {
                        world.scheduleUpdate(below, originState.getBlock(), tickRate);
                        quantaRemaining = amt - originState.getQuantaPerBlock();
                    }
                }
                else if(amt > 0) {
                    if(helper.flowInto(0, 0, 0, amt - 1, originState.getDownDensityFace(), false, true, Constants.BlockFlags.DEFAULT)) {
                        world.scheduleUpdate(below, originState.getBlock(), tickRate);
                        FluidloggedUtils.setFluidToAir(world, origin, null, Constants.BlockFlags.DEFAULT);
                        return;
                    }
                }
            }

            // flow vertically if possible
            else {
                final int density_other = BlockFluidBase.getDensity(helper.getCache(), below);
                if(density_other == Integer.MAX_VALUE) {
                    if(helper.flowInto(0, 0, 0, quantaRemaining - 1, originState.getDownDensityFace(), true, true, Constants.BlockFlags.DEFAULT)) {
                        world.scheduleUpdate(below, originState.getBlock(), tickRate);
                        FluidloggedUtils.setFluidToAir(world, origin, null, Constants.BlockFlags.DEFAULT);
                        return;
                    }
                }
                // swap this with fluid below
                else if(helper.canFluidFlow(0, -1, 0, originState.getUpDensityFace())) {
                    if(originState.getDensityDir() < 0
                            ? density_other < originState.getDensity()
                            : density_other > originState.getDensity()) {
                        @Nonnull final FluidState fluidToSwap = helper.getFluidState(0, -1, 0);
                        if(fluidToSwap.isValid() && !(fluidToSwap.getBlock() instanceof IConditionalFluid
                        && ((IConditionalFluid)fluidToSwap.getBlock()).cannotFlowAt(helper.getCache(), origin, fluidToSwap))) {
                            // ensure top can swap with bottom
                            if(helper.getBlockState(0, -1, 0) == fluidToSwap.getState()
                            || helper.isReplaceable(0, -1, 0, originState.withLevel(quantaRemaining - 1), null, false, true)
                            || helper.isFluidloggable(0, -1, 0, originState.withLevel(quantaRemaining - 1), null, false, true)) {
                                @Nonnull final IFluidUpdateHelper toSwapHelper = fluidToSwap.getBlock() instanceof BlockLiquid
                                        ? new IFluidUpdateHelper.Vanilla(helper.getCache(), origin, fluidToSwap, 1)
                                        : new IFluidUpdateHelper.Forge(helper.getCache(), origin, fluidToSwap, 1);
                                // ensure bottom can swap with top
                                if(toSwapHelper.getBlockState(0, 0, 0) == originState.getState()
                                || toSwapHelper.isReplaceable(0, 0, 0, fluidToSwap, null, false, true)
                                || toSwapHelper.isFluidloggable(0, 0, 0, fluidToSwap, null, false, true)) {
                                    // do swap
                                    helper.flowInto(0, 0, 0, quantaRemaining - 1, originState.getDownDensityFace(), false, true, Constants.BlockFlags.DEFAULT);
                                    toSwapHelper.flowInto(0, -1, 0, fluidToSwap.getLevel(), originState.getUpDensityFace(), false, true, Constants.BlockFlags.DEFAULT);
                                    world.scheduleUpdate(below, originState.getBlock(), tickRate);
                                    world.scheduleUpdate(origin, fluidToSwap.getBlock(), fluidToSwap.getBlock().tickRate(world));
                                    return;
                                }
                            }
                        }
                    }
                }
            }

            if(quantaRemaining < 1) return;
            else if(quantaRemaining != prevRemaining) {
                changed = true;
                if(quantaRemaining == 1) helper.setFluid(0, 0, 0, originState.withLevel(0), false, 0);
            }
            else if(quantaRemaining == 1) return;
        }
        else if(quantaRemaining == 1) return;

        // --------------------
        // flow out if possible
        // --------------------

        int lowerThan = quantaRemaining - 1;
        int total = quantaRemaining;
        int count = 1;

        @Nonnull final List<EnumFacing> sides = new ArrayList<>();
        for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
            if(helper.canFlowInto(0, 0, 0, lowerThan, side, true, false)) {
                sides.add(side);
                count++;
                total += Math.max(originState.getQuantaValueBelow(helper.getCache(), origin.offset(side), lowerThan), 0);
            }
        }

        if(count == 1) {
            if(changed) helper.setFluid(0, 0, 0, originState.withLevel(quantaRemaining - 1), false, 0);
            return;
        }

        int each = total / count;
        int rem = total % count;

        for(@Nonnull final EnumFacing side : sides) {
            @Nonnull final BlockPos off = origin.offset(side);

            int newQuanta = each;
            if(rem == count || rem > 1 && rand.nextInt(count - rem) != 0) {
                ++newQuanta;
                --rem;
            }

            final int quanta = Math.max(0, originState.getQuantaValue(helper.getCache(), off));
            if(quanta < lowerThan && newQuanta != quantaRemaining) {
                if(newQuanta == 0) FluidloggedUtils.setFluidToAir(world, off, helper.getBlockState(side.getXOffset(), 0, side.getZOffset()), Constants.BlockFlags.DEFAULT);
                else if(helper.flowInto(0, 0, 0, newQuanta - 1, side, true, false, Constants.BlockFlags.SEND_TO_CLIENTS)) {
                    world.scheduleUpdate(off, originState.getBlock(), tickRate);
                    changed = true;
                }
            }
            --count;
        }

        if(rem > 0) ++each;
        if(changed) helper.setFluid(0, 0, 0, originState.withLevel(each - 1), false, 0);
    }
}
