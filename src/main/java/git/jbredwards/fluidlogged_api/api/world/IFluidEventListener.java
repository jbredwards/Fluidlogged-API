/*
 * Copyright (C) <2026 to Present> <jbredwards>
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

package git.jbredwards.fluidlogged_api.api.world;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;

/**
 * Allows code to be executed when a FluidState changes somewhere in the world.
 * This is intended to be used the same way as {@link net.minecraft.world.IWorldEventListener#notifyBlockUpdate IWorldEventListener.notifyBlockUpdate()}.
 *
 * @since 3.3.0
 * @see git.jbredwards.fluidlogged_api.api.block.IFluidloggable#onFluidChange
 * @author jbred
 *
 */
public interface IFluidEventListener
{
    /**
     * All {@code IFluidEventListener} instances.
     * @since 3.3.0
     */
    @Nonnull
    List<IFluidEventListener> LISTENERS = new ArrayList<>();

    /**
     * Notifies this listener that the FluidState at the provided position has changed.
     * @throws NullPointerException If any parameters are null.
     * @since 3.3.0
     */
    void notifyFluidUpdate(@Nonnull final Chunk chunk, @Nonnull final BlockPos pos, @Nonnull final FluidState oldState, @Nonnull final FluidState newState, final int blockFlags);
}
