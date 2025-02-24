/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.fluid;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.event.FluidloggableEvent;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;

/**
 * Have your fluid block implement this if it should be able to hold fluidloggable blocks.
 * Note that {@link net.minecraft.block.BlockLiquid}, {@link net.minecraftforge.fluids.BlockFluidClassic},
 * and {@link net.minecraftforge.fluids.BlockFluidFinite} already implement this at runtime.
 * <p>Use {@link net.minecraftforge.fml.common.Optional Forge's Optional @interfaces} to prevent a required Fluidlogged API dependency.</p>
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public interface IFluidloggableFluid
{
    /**
     * @param fluidState FluidState.
     * @return True if this fluid is fluidloggable. This method does not check the block state to be fluidlogged,
     * for that call {@link IFluidloggableFluid#isStateFluidloggable isStateFluidloggable}.
     *
     * @throws NullPointerException If fluidState is null.
     * @since 3.0.0
     * @author jbred
     */
    default boolean isFluidloggableFluid(@Nonnull final FluidState fluidState) {
        return fluidState.isValid() && !fluidState.getBlock().hasTileEntity(fluidState.getState());
    }

    /**
     * @param world World.
     * @param fluidState This FluidState.
     * @param other Other FluidState.
     * @param allowMatching True if this should allow replacing of same fluids.
     * @return True if this fluid can be replaced by other, based on fluid quanta and density.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @throws IllegalArgumentException If either of the FluidState parameters are empty.
     * @since 3.0.0
     * @author jbred
     */
    default boolean isReplaceableByOther(@Nonnull final World world, @Nonnull final FluidState fluidState, @Nonnull final FluidState other, final boolean allowMatching) {
        if(allowMatching && FluidloggedUtils.isCompatibleFluid(fluidState, other) || other.isSource()) return true;
        else if(fluidState.isSource()) return false;

        final int hereDensity = fluidState.withLevel(fluidState.getWrappedLevel(world)).getQuantaValue() * fluidState.getDensity();
        final int otherDensity = other.withLevel(other.getWrappedLevel(world)).getQuantaValue() * other.getDensity();

        return allowMatching ? hereDensity <= otherDensity : hereDensity < otherDensity;
    }

    /**
     * @param state IBlockState.
     * @param world World.
     * @param pos Position.
     * @param fluidState FluidState.
     * @return True if state is fluidloggable with this fluid.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     * @author jbred
     */
    default boolean isStateFluidloggable(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
        if(state.getBlock().isAir(state, world, pos) || state.getBlock() instanceof IFluidloggableFluid || FluidloggedUtils.isFluid(state)) return false;

        // event (configs are also called through this)
        @Nonnull final FluidloggableEvent event = new FluidloggableEvent(state, world, pos, fluidState);
        MinecraftForge.EVENT_BUS.post(event);
        if(event.getResult() != Event.Result.DEFAULT) return event.getResult() == Event.Result.ALLOW;

        // defaults
        else return state.getBlock() instanceof IFluidloggable && (FluidloggedAPIConfig.allowDefaults
                    || ((IFluidloggable)state.getBlock()).overrideApplyDefaultsSetting())
                    && ((IFluidloggable)state.getBlock()).isFluidloggable(state, world, pos, fluidState);
    }
}
