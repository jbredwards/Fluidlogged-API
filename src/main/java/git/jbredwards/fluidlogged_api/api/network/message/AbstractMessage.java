/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.network.message;

import io.netty.buffer.ByteBuf;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;

import javax.annotation.Nonnull;

/**
 * An IMessage base class that uses PacketBuffer and has "isValid" checks built-in.
 * @since 1.9.0
 * @author jbred
 *
 */
public abstract class AbstractMessage implements IMessage
{
    /**
     * Controls whether this message is written to the buffer and handled by the
     * {@link net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler IMessageHandler}.
     * This value should be set to true in your message's non-empty constructor only.
     * @since 1.9.0
     */
    public boolean isValid;

    /**
     * Convert from the supplied buffer into this specific message type if the message is valid.
     *
     * @param buf The byte buffer.
     * @throws NullPointerException If buf is null.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Override
    public final void fromBytes(@Nonnull final ByteBuf buf) {
        isValid = buf.readBoolean();
        if(isValid) read(new PacketBuffer(buf));
    }

    /**
     * Deconstructs this message into the supplied byte buffer if this message is valid.
     *
     * @param buf The byte buffer.
     * @throws NullPointerException If buf is null.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Override
    public final void toBytes(@Nonnull final ByteBuf buf) {
        buf.writeBoolean(isValid);
        if(isValid) write(new PacketBuffer(buf));
    }

    /**
     * Convert from the supplied packet buffer into your specific message type.
     *
     * @param buf The packet buffer.
     * @throws NullPointerException If buf is null.
     *
     * @since 1.9.0.3
     * @author jbred
     */
    public abstract void read(@Nonnull final PacketBuffer buf);

    /**
     * Deconstructs your message into the supplied packet buffer.
     *
     * @param buf The packet buffer.
     * @throws NullPointerException If buf is null.
     *
     * @since 1.9.0.3
     * @author jbred
     */
    public abstract void write(@Nonnull final PacketBuffer buf);
}
