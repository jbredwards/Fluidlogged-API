/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;

/**
 * play fluid vaporize effects for the client from the server
 * @author jbred
 *
 */
public final class SMessageVaporizeEffects extends AbstractMessage
{
    public FluidState fluid;
    public BlockPos pos;

    public SMessageVaporizeEffects() {}
    public SMessageVaporizeEffects(@Nonnull FluidState fluidIn, @Nonnull BlockPos posIn) {
        fluid = fluidIn;
        pos = posIn;
        isValid = true;
    }

    @Override
    public void read(@Nonnull PacketBuffer buf) {
        fluid = FluidState.deserialize(buf.readVarInt());
        pos = buf.readBlockPos();
    }

    @Override
    public void write(@Nonnull PacketBuffer buf) {
        buf.writeVarInt(fluid.serialize());
        buf.writeBlockPos(pos);
    }

    public enum Handler implements IClientMessageHandler<SMessageVaporizeEffects>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull final SMessageVaporizeEffects message, @Nonnull final MessageContext ctx) {
            message.fluid.getFluid().vaporize(null, IClientMessageHandler.getWorldFromContext(ctx), message.pos, message.fluid.createFluidStack());
        }
    }
}
