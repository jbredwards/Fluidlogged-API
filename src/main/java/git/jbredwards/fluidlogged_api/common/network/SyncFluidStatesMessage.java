package git.jbredwards.fluidlogged_api.common.network;

import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import io.netty.buffer.ByteBuf;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * syncs all fluid states in a chunk
 * @author jbred
 *
 */
public final class SyncFluidStatesMessage implements IMessage
{
    @Nonnull public Set<Pair<Long, Integer>> data = new HashSet<>();
    public int x, z;

    @SuppressWarnings("unused")
    public SyncFluidStatesMessage() {}
    public SyncFluidStatesMessage(ChunkPos pos, @Nonnull Map<BlockPos, FluidState> fluidStateMap) {
        x = pos.x;
        z = pos.z;

        for(Map.Entry<BlockPos, FluidState> entry : fluidStateMap.entrySet()) {
            data.add(Pair.of(entry.getKey().toLong(), entry.getValue().serialize()));
        }
    }

    @Override
    public void fromBytes(ByteBuf buf) {
        if(buf.readBoolean()) {
            //read pos
            x = buf.readInt();
            z = buf.readInt();
            //read data
            int size = buf.readInt();
            for(int i = 0; i < size; i++) data.add(Pair.of(buf.readLong(), buf.readInt()));
        }
    }

    @Override
    public void toBytes(ByteBuf buf) {
        if(data.isEmpty()) buf.writeBoolean(false);
        else {
            buf.writeBoolean(true);
            //write pos
            buf.writeInt(x);
            buf.writeInt(z);
            //write data
            buf.writeInt(data.size());
            data.forEach(entry -> {
                buf.writeLong(entry.getKey());
                buf.writeInt(entry.getValue());
            });
        }
    }

    public enum Handler implements IMessageHandler<SyncFluidStatesMessage, IMessage>
    {
        INSTANCE;

        @Nullable
        @Override
        public IMessage onMessage(SyncFluidStatesMessage message, MessageContext ctx) {
            if(!message.data.isEmpty() && ctx.side == Side.CLIENT) {
                Minecraft.getMinecraft().addScheduledTask(() -> {
                    final WorldClient world = Minecraft.getMinecraft().world;
                    final IFluidStateCapability cap = IFluidStateCapability.get(
                            world.getChunkFromChunkCoords(message.x, message.z));

                    if(cap != null) {
                        //clear all old fluid states
                        cap.getFluidStates().clear();

                        //set all new fluid states
                        for(Pair<Long, Integer> entry : message.data) {
                            BlockPos pos = BlockPos.fromLong(entry.getKey());

                            //send changes to client
                            cap.setFluidState(pos, FluidState.deserialize(entry.getValue()));

                            //update light levels
                            world.profiler.startSection("checkLight");
                            world.checkLight(pos);
                            world.profiler.endSection();

                            //re-render block
                            world.markBlockRangeForRenderUpdate(pos, pos);
                        }
                    }
                });
            }

            return null;
        }
    }
}
