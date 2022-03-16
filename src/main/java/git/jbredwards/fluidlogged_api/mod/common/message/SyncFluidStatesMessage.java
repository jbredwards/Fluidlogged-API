package git.jbredwards.fluidlogged_api.mod.common.message;

import com.google.common.collect.ImmutableSet;
import git.jbredwards.fluidlogged_api.mod.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import io.netty.buffer.ByteBuf;
import net.minecraft.client.Minecraft;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.World;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * syncs all FluidStates in a chunk
 * @author jbred
 *
 */
public final class SyncFluidStatesMessage implements IMessage
{
    @Nonnull public Set<Pair<Long, Integer>> data = new HashSet<>();
    public boolean isValid;
    public int chunkX, chunkZ;

    public SyncFluidStatesMessage() {}
    public SyncFluidStatesMessage(@Nonnull ChunkPos chunkPos, @Nonnull Map<BlockPos, FluidState> fluidStateMap) {
        fluidStateMap.forEach((pos, fluidState) -> data.add(Pair.of(pos.toLong(), fluidState.serialize())));
        chunkX = chunkPos.x;
        chunkZ = chunkPos.z;
        isValid = true;
    }

    @Override
    public void fromBytes(ByteBuf buf) {
        isValid = buf.readBoolean();
        if(isValid) {
            //read pos
            chunkX = buf.readInt();
            chunkZ = buf.readInt();
            //read data
            final int size = buf.readInt();
            for(int i = 0; i < size; i++) data.add(Pair.of(buf.readLong(), buf.readInt()));
        }
    }

    @Override
    public void toBytes(ByteBuf buf) {
        buf.writeBoolean(isValid);
        if(isValid) {
            //write pos
            buf.writeInt(chunkX);
            buf.writeInt(chunkZ);
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
        public IMessage onMessage(@Nonnull SyncFluidStatesMessage message, @Nonnull MessageContext ctx) {
            if(message.isValid && ctx.side.isClient()) addTask(message);
            return null;
        }

        @SideOnly(Side.CLIENT)
        private void addTask(@Nonnull SyncFluidStatesMessage message) {
            Minecraft.getMinecraft().addScheduledTask(() -> {
                final World world = Minecraft.getMinecraft().world;
                final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(
                        world.getChunkFromChunkCoords(message.chunkX, message.chunkZ));

                if(cap != null) {
                    //clear any old fluid states
                    final Set<BlockPos> removed = ImmutableSet.copyOf(cap.getFluidStates().keySet());
                    cap.getFluidStates().clear();

                    //add any new fluid states
                    message.data.forEach(entry -> {
                        BlockPos pos = BlockPos.fromLong(entry.getKey());
                        FluidState fluidState = FluidState.deserialize(entry.getValue());

                        //send changes to client
                        cap.setFluidState(pos, fluidState);

                        //re-render block
                        FluidloggedUtils.relightFluidBlock(world, pos, fluidState);
                        world.markBlockRangeForRenderUpdate(pos, pos);
                    });

                    //update removed light levels & renders
                    removed.forEach(pos -> {
                        //make sure the cleared pos wasn't replaced prior to re-render
                        if(!cap.getFluidStates().containsKey(pos)) {
                            //re-render block
                            FluidloggedUtils.relightFluidBlock(world, pos, FluidState.EMPTY);
                            world.markBlockRangeForRenderUpdate(pos, pos);
                        }
                    });
                }
            });
        }
    }
}
