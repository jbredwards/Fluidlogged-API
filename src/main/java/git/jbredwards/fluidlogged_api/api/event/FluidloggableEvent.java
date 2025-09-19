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

package git.jbredwards.fluidlogged_api.api.event;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.world.IWorldProvider;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * This event is fired on the {@link net.minecraftforge.common.MinecraftForge#EVENT_BUS}, through
 * {@link git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid#isStateFluidloggable IFluidloggableFluid.isStateFluidloggable()}.
 * For modded blocks it's recommended to implement {@link git.jbredwards.fluidlogged_api.api.block.IFluidloggable IFluidloggable}
 * instead of using this event, as it also provides more functionality.<br>
 * <br>
 * This event is {@link Cancelable cancelable}.<br>
 * This event has a {@link HasResult result}:
 * <li>{@link Result#ALLOW} means that the state is fluidloggable.</li>
 * <li>{@link Result#DEFAULT} means that {@link git.jbredwards.fluidlogged_api.api.block.IFluidloggable IFluidloggable} will be checked.</li>
 * <li>{@link Result#DENY} means that the state is not fluidloggable.</li>
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
    @Nonnull public final BlockPos pos;
    @Deprecated
    @Nonnull public final World world; // deprecated since 3.0.0
    @Nonnull public final IBlockAccess access; // since 3.0.0

    @Nullable public final Fluid fluid;
    @Nonnull public final FluidState fluidState; // since 3.0.0

    public FluidloggableEvent(@Nonnull final IBlockState stateIn, @Nonnull final IBlockAccess accessIn, @Nonnull final BlockPos posIn, @Nonnull final FluidState fluidStateIn) {
        state = stateIn;
        pos = posIn;

        fluidState = fluidStateIn;
        fluid = fluidStateIn.getFluid();

        access = accessIn;
        world = IWorldProvider.getWorld(accessIn);
    }
}
