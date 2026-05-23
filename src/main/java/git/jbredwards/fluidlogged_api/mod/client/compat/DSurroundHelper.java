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

package git.jbredwards.fluidlogged_api.mod.client.compat;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.world.IFluidEventListener;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.orecruncher.dsurround.event.BlockUpdateEvent;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class DSurroundHelper
{
    @SideOnly(Side.CLIENT)
    public static void registerListener() { IFluidEventListener.LISTENERS.add(Listener.INSTANCE); }
    public enum Listener implements IFluidEventListener
    {
        INSTANCE;

        @Override
        public void notifyFluidUpdate(@Nonnull final Chunk chunk, @Nonnull final BlockPos pos, @Nonnull final FluidState oldState, @Nonnull final FluidState newState, final int flags) {
            if((flags & Constants.BlockFlags.SEND_TO_CLIENTS) != 0 && (!chunk.getWorld().isRemote || (flags & Constants.BlockFlags.NO_RERENDER) == 0) && chunk.isPopulated())
                MinecraftForge.EVENT_BUS.post(new BlockUpdateEvent(chunk.getWorld(), pos, oldState.getState(), newState.getState(), flags));
        }
    }
}
