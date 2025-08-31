/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.network;

import com.google.gson.JsonElement;
import com.google.gson.internal.bind.TypeAdapters;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.ByteBufInputStream;
import io.netty.buffer.ByteBufOutputStream;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.server.management.PlayerChunkMapEntry;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.WorldServer;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.*;
import java.nio.charset.StandardCharsets;
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
     * Reads a JsonElement with UTF8 byte encoding from the buffer.
     * @param buf The buffer to read from.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 3.1.0
     * @author jbred
     */
    @Nonnull
    public static JsonElement readJson(@Nonnull final ByteBuf buf) {
        @Nonnull final InputStream is = new ByteBufInputStream(buf, buf.readInt());
        try(@Nonnull final Reader reader = new InputStreamReader(is, StandardCharsets.UTF_8)) {
            return TypeAdapters.JSON_ELEMENT.fromJson(reader);
        }

        catch(@Nonnull final IOException e) { throw new RuntimeException(e); } // Unpossible?
    }

    /**
     * Writes a JsonElement with UTF8 byte encoding to the buffer.
     * @param buf The buffer to write to.
     * @param json The JsonElement to write.
     *
     * @throws NullPointerException If any parameters are null.
     * @since 3.1.0
     * @author jbred
     */
    @Nonnull
    public static ByteBuf writeJson(@Nonnull final ByteBuf buf, @Nonnull final JsonElement json) {
        final int startIndex = buf.writerIndex();

        @Nonnull final OutputStream os = new ByteBufOutputStream(buf.writeInt(0)); // Allocate size int.
        try(@Nonnull final Writer writer = new OutputStreamWriter(os, StandardCharsets.UTF_8)) {
            TypeAdapters.JSON_ELEMENT.toJson(writer, json);
        }

        catch(@Nonnull final IOException e) { throw new RuntimeException(e); } // Unpossible?
        return buf.setInt(startIndex, buf.writerIndex() - startIndex - 4); // Write size int to start index.
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
