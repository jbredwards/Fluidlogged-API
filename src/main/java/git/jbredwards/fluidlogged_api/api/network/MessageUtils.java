package git.jbredwards.fluidlogged_api.api.network;

import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.server.management.PlayerChunkMapEntry;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.WorldServer;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;

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
    public static void sendToAllTracking(@Nonnull IMessage message, @Nonnull Chunk chunk, @Nonnull BiConsumer<IMessage, EntityPlayerMP> networkWrapper) {
        if(chunk.getWorld() instanceof WorldServer) {
            final PlayerChunkMapEntry entry = ((WorldServer)chunk.getWorld()).getPlayerChunkMap().getEntry(chunk.x, chunk.z);
            if(entry != null) entry.getWatchingPlayers().forEach(player -> networkWrapper.accept(message, player));
        }
    }

    /**
     * Send a packet to all players tracking a TileEntity
     */
    public static void sendToAllTracking(@Nonnull IMessage message, @Nonnull TileEntity tile, @Nonnull BiConsumer<IMessage, EntityPlayerMP> networkWrapper) {
        if(tile.hasWorld() && tile.getWorld() instanceof WorldServer) {
            final int x = tile.getPos().getX() >> 4, z = tile.getPos().getZ() >> 4;
            final PlayerChunkMapEntry entry = ((WorldServer)tile.getWorld()).getPlayerChunkMap().getEntry(x, z);
            if(entry != null) entry.getWatchingPlayers().forEach(player -> networkWrapper.accept(message, player));
        }
    }
}
