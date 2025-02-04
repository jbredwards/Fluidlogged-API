/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.event;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Fired through {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils#isStateFluidloggable FluidloggedUtils.isStateFluidloggable()}.
 * For modded blocks it's recommended to implement {@link git.jbredwards.fluidlogged_api.api.block.IFluidloggable IFluidloggable} instead of using this event,
 * it also provides more functionality.
 *
 * @since 1.9.0
 * @author jbred
 *
 */
@Cancelable
@Event.HasResult
public class FluidloggableEvent extends Event
{
    @Nonnull public final IBlockState state;
    @Nonnull public final IBlockAccess world;
    @Nonnull public final BlockPos pos;

    @Nullable public final Fluid fluid;
    @Nonnull public final FluidState fluidState; // since 3.0.0

    public FluidloggableEvent(@Nonnull final IBlockState stateIn, @Nonnull final IBlockAccess worldIn, @Nonnull final BlockPos posIn, @Nonnull final FluidState fluidStateIn) {
        state = stateIn;
        world = worldIn;
        pos = posIn;

        fluidState = fluidStateIn;
        fluid = fluidStateIn.getFluid();
    }
}
