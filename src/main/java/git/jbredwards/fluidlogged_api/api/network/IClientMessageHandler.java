/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.network;

import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * A client-side {@link IMessageHandler} that handles {@link AbstractMessage AbstractMessages} in a thread-safe way.
 *
 * @since 1.9.0
 * @author jbred
 *
 */
public interface IClientMessageHandler<REQ extends AbstractMessage> extends IMessageHandler<REQ, IMessage>
{
    /**
     * Called when a message is received of the appropriate type. This method should generally not be overridden,
     * otherwise just create your own IMessageHandler :P.
     *
     * @param message The message
     * @param ctx The message context
     * @return An optional return message, in this case none.
     * @throws NullPointerException If message or ctx are null.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nullable
    @Override
    default IMessage onMessage(@Nonnull final REQ message, @Nonnull final MessageContext ctx) {
        if(message.isValid && ctx.side.isClient()) FMLClientHandler.instance().getClient().addScheduledTask(() -> {
            //only handle message if client is definitely still in the server (issue#204)
            //noinspection ConstantValue
            if(getWorldFromContext(ctx) != null) handleMessage(message, ctx);
            return null;
        });

        return null;
    }

    /**
     * Use the context-sensitive {@link IClientMessageHandler#handleMessage(AbstractMessage, MessageContext)} method instead.
     *
     * @param message The message.
     * @throws NullPointerException If message is null.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Deprecated
    @SideOnly(Side.CLIENT)
    default void handleMessage(@Nonnull final REQ message) {
        // NO-OP
    }

    /**
     * Called when a message is received of the appropriate type. This method is thread-safe.
     *
     * @param message The message.
     * @param ctx The message context.
     * @throws NullPointerException If message or ctx are null.
     *
     * @since 3.0.0
     * @author jbred
     */
    @SideOnly(Side.CLIENT)
    default void handleMessage(@Nonnull final REQ message, @Nonnull final MessageContext ctx) throws Exception {
        handleMessage(message);
    }

    /**
     * Should always be used in place of calling {@link net.minecraft.client.Minecraft#world Minecraft.world} directly.
     *
     * @param ctx MessageContext.
     * @return WorldClient instance provided by the MessageContext's client handler.
     * @throws NullPointerException If ctx is null.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    @SideOnly(Side.CLIENT)
    static WorldClient getWorldFromContext(@Nonnull final MessageContext ctx) {
        return ctx.getClientHandler().world;
    }
}
