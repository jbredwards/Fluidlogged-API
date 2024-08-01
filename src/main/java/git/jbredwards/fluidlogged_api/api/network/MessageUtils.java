/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.network;

import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.server.management.PlayerChunkMapEntry;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.WorldServer;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.function.BiConsumer;

/**
 * An IMessage utility class.
 *
 * @since 1.9.0.5
 * @author jbred
 *
 */
public final class MessageUtils
{
    /**
     * Sends an IMessage to all players tracking a Chunk.
     *
     * @param message The message.
     * @param chunk The chunk.
     * @param networkWrapper Network handler.
     * @throws NullPointerException If any parameters are null.
     *
     * @since 1.9.0.5
     * @author jbred
     */
    public static void sendToAllTracking(@Nonnull final IMessage message, @Nonnull final Chunk chunk, @Nonnull final SimpleNetworkWrapper networkWrapper) {
        sendToAllTracking(message, chunk, networkWrapper::sendTo);
    }

    /**
     * Sends an IMessage to all players tracking a Chunk.
     *
     * @param message The message.
     * @param chunk The chunk.
     * @param networkWrapper Network handler.
     * @throws NullPointerException If any parameters are null.
     *
     * @since 1.9.0.5
     * @author jbred
     */
    public static void sendToAllTracking(@Nonnull final IMessage message, @Nonnull final Chunk chunk, @Nonnull final BiConsumer<IMessage, EntityPlayerMP> networkWrapper) {
        if(chunk.getWorld() instanceof WorldServer) {
            @Nullable final PlayerChunkMapEntry entry = ((WorldServer)chunk.getWorld()).getPlayerChunkMap().getEntry(chunk.x, chunk.z);
            if(entry != null) entry.getWatchingPlayers().forEach(player -> networkWrapper.accept(message, player));
        }
    }

    /**
     * Sends an IMessage to all players tracking a TileEntity.
     *
     * @param message The message.
     * @param tile The tile entity.
     * @param networkWrapper Network handler.
     * @throws NullPointerException If any parameters are null.
     *
     * @since 1.9.0.5
     * @author jbred
     */
    public static void sendToAllTracking(@Nonnull final IMessage message, @Nonnull final TileEntity tile, @Nonnull final SimpleNetworkWrapper networkWrapper) {
        sendToAllTracking(message, tile, networkWrapper::sendTo);
    }

    /**
     * Sends an IMessage to all players tracking a TileEntity.
     *
     * @param message The message.
     * @param tile The tile entity.
     * @param networkWrapper Network handler.
     * @throws NullPointerException If any parameters are null.
     *
     * @since 1.9.0.5
     * @author jbred
     */
    public static void sendToAllTracking(@Nonnull final IMessage message, @Nonnull final TileEntity tile, @Nonnull final BiConsumer<IMessage, EntityPlayerMP> networkWrapper) {
        if(tile.hasWorld() && tile.getWorld() instanceof WorldServer) {
            @Nullable final PlayerChunkMapEntry entry = ((WorldServer)tile.getWorld()).getPlayerChunkMap().getEntry(tile.getPos().getX() >> 4, tile.getPos().getZ() >> 4);
            if(entry != null) entry.getWatchingPlayers().forEach(player -> networkWrapper.accept(message, player));
        }
    }
}
