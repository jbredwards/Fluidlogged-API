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

package git.jbredwards.fluidlogged_api.api.network;

import com.google.common.util.concurrent.Futures;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * A server-side {@link IMessageHandler} that handles {@link AbstractMessage AbstractMessages} in a thread-safe way.
 *
 * @since 3.1.0
 * @author jbred
 *
 */
public interface IServerMessageHandler<REQ extends AbstractMessage, REPLY extends IMessage> extends IMessageHandler<REQ, REPLY>
{
    /**
     * Called when a message is received of the appropriate type. This method should generally not be overridden,
     * otherwise just create your own IMessageHandler :P.
     *
     * @param message The message
     * @param ctx The message context
     * @return An optional return message.
     * @throws NullPointerException If message or ctx are null.
     *
     * @since 3.1.0
     * @author jbred
     */
    @Nullable
    @Override
    default REPLY onMessage(@Nonnull final REQ message, @Nonnull final MessageContext ctx) {
        return message.isValid && ctx.side.isServer() ? Futures.getUnchecked(FMLCommonHandler.instance()
                .getMinecraftServerInstance().callFromMainThread(() -> handleMessage(message, ctx))) : null;
    }

    /**
     * Called when a message is received of the appropriate type. This method is thread-safe.
     *
     * @param message The message.
     * @param ctx The message context.
     * @throws NullPointerException If message or ctx are null.
     *
     * @since 3.1.0
     * @author jbred
     */
    @Nullable
    REPLY handleMessage(@Nonnull final REQ message, @Nonnull final MessageContext ctx) throws Exception;
}
