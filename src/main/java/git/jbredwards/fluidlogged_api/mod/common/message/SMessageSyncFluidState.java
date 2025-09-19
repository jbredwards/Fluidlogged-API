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

package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * send a FluidState to the client
 * @author jbred
 *
 */
public final class SMessageSyncFluidState extends AbstractMessage
{
    public BlockPos pos;
    public FluidState state;
    public boolean doRenderUpdate;

    public SMessageSyncFluidState() {}
    public SMessageSyncFluidState(@Nonnull final BlockPos posIn, @Nonnull final FluidState stateIn, final boolean doRenderUpdateIn) {
        isValid = true;
        pos = posIn;
        state = stateIn;
        doRenderUpdate = doRenderUpdateIn;
    }

    @Override
    public void read(@Nonnull final PacketBuffer buf) {
        pos = buf.readBlockPos();
        state = FluidState.deserialize(buf.readVarInt());
        doRenderUpdate = buf.readBoolean();
    }

    @Override
    public void write(@Nonnull final PacketBuffer buf) {
        buf.writeBlockPos(pos).writeVarInt(state.serialize()).writeBoolean(doRenderUpdate);
    }

    public enum Handler implements IClientMessageHandler<SMessageSyncFluidState>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull final SMessageSyncFluidState message, @Nonnull final MessageContext ctx) {
            @Nonnull final World world = IClientMessageHandler.getWorldFromContext(ctx);
            @Nonnull final Chunk chunk = world.getChunk(message.pos);
            @Nullable final IFluidStateCapability cap = IFluidStateCapability.get(chunk);

            if(cap != null) {
                // send changes to client
                cap.getContainer(message.pos.getY()).setFluidState(message.pos, message.state);
                // re-render block
                FluidloggedUtils.relightFluidBlock(world, message.pos, chunk, message.state);
                if(message.doRenderUpdate) world.markBlockRangeForRenderUpdate(message.pos, message.pos);
            }
        }
    }
}
