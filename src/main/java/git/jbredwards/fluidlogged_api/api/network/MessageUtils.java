package git.jbredwards.fluidlogged_api.api.network;

import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.server.management.PlayerChunkMapEntry;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.WorldServer;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;

import javax.annotation.Nonnull;
import java.util.function.BiConsumer;

/**
 * General packet utilities
 * @author jbred
 *
 */
public final class MessageUtils
{
    /**
     * Send a packet to all players tracking a Chunk
     */
    public static void sendToAllTracking(@Nonnull IMessage message, @Nonnull Chunk chunk, @Nonnull SimpleNetworkWrapper networkWrapper) { sendToAllTracking(message, chunk, networkWrapper::sendTo); }
    public static void sendToAllTracking(@Nonnull IMessage message, @Nonnull Chunk chunk, @Nonnull BiConsumer<IMessage, EntityPlayerMP> networkWrapper) {
        if(chunk.getWorld() instanceof WorldServer) {
            final PlayerChunkMapEntry entry = ((WorldServer)chunk.getWorld()).getPlayerChunkMap().getEntry(chunk.x, chunk.z);
            if(entry != null) entry.getWatchingPlayers().forEach(player -> networkWrapper.accept(message, player));
        }
    }

    /**
     * Send a packet to all players tracking a TileEntity
     */
    public static void sendToAllTracking(@Nonnull IMessage message, @Nonnull TileEntity tile, @Nonnull SimpleNetworkWrapper networkWrapper) { sendToAllTracking(message, tile, networkWrapper::sendTo); }
    public static void sendToAllTracking(@Nonnull IMessage message, @Nonnull TileEntity tile, @Nonnull BiConsumer<IMessage, EntityPlayerMP> networkWrapper) {
        if(tile.hasWorld() && tile.getWorld() instanceof WorldServer) {
            final PlayerChunkMapEntry entry = ((WorldServer)tile.getWorld()).getPlayerChunkMap().getEntry(tile.getPos().getX() >> 4, tile.getPos().getZ() >> 4);
            if(entry != null) entry.getWatchingPlayers().forEach(player -> networkWrapper.accept(message, player));
        }
    }
}
