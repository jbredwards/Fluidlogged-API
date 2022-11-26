package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import io.netty.buffer.ByteBuf;
import it.unimi.dsi.fastutil.chars.CharOpenHashSet;
import it.unimi.dsi.fastutil.chars.CharSet;
import net.minecraft.client.Minecraft;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

/**
 * syncs all FluidStates in a chunk
 * @author jbred
 *
 */
public final class MessageSyncFluidStates extends AbstractMessage
{
    @Nonnull
    public List<Pair<Character, Integer>> data = new ArrayList<>();
    public int x, y, z;

    public MessageSyncFluidStates() {}
    public MessageSyncFluidStates(@Nonnull Chunk chunk, @Nonnull IFluidStateCapability cap) {
        this(chunk.x, 0, chunk.z, cap);
    }

    public MessageSyncFluidStates(int chunkXIn, int chunkYIn, int chunkZIn, @Nonnull IFluidStateCapability cap) {
        cap.getContainer(chunkXIn, chunkYIn, chunkZIn).forEach((pos, fluidState)
                -> data.add(Pair.of(pos, fluidState.serialize())));

        x = chunkXIn;
        y = chunkYIn;
        z = chunkZIn;
        isValid = true;
    }

    @Override
    public void read(@Nonnull ByteBuf buf) {
        //read pos
        x = buf.readInt();
        y = buf.readInt();
        z = buf.readInt();
        //read data
        final int size = buf.readInt();
        for(int i = 0; i < size; i++) data.add(Pair.of(buf.readChar(), buf.readInt()));
    }

    @Override
    public void write(@Nonnull ByteBuf buf) {
        //write pos
        buf.writeInt(x);
        buf.writeInt(y);
        buf.writeInt(z);
        //write data
        buf.writeInt(data.size());
        data.forEach(entry -> {
            buf.writeChar(entry.getKey());
            buf.writeInt(entry.getValue());
        });
    }

    public enum Handler implements IClientMessageHandler<MessageSyncFluidStates>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull MessageSyncFluidStates message) {
            final World world = Minecraft.getMinecraft().world;
            final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(world.getChunk(message.x, message.z));

            if(cap != null) {
                final IFluidStateContainer container = cap.getContainer(message.x, message.y, message.z);
                //clear any old fluid states
                final CharSet removed = new CharOpenHashSet();
                container.forEach((pos, fluidState) -> removed.add(pos));
                container.clearFluidStates();
                //add any new fluid states
                message.data.forEach(entry -> {
                    final BlockPos pos = container.deserializePos(entry.getKey());
                    final FluidState fluidState = FluidState.deserialize(entry.getValue());
                    //send changes to client
                    container.setFluidState(entry.getKey(), fluidState);
                    //re-render block
                    world.markBlockRangeForRenderUpdate(pos, pos);
                    FluidloggedUtils.relightFluidBlock(world, pos, fluidState);
                });
                //update removed light levels & renders
                removed.forEach(serializedPos -> {
                    //make sure the cleared pos wasn't replaced prior to re-render
                    if(!container.hasFluidState(serializedPos)) {
                        //re-render block
                        final BlockPos pos = container.deserializePos(serializedPos);
                        world.markBlockRangeForRenderUpdate(pos, pos);
                        FluidloggedUtils.relightFluidBlock(world, pos, FluidState.EMPTY);
                    }
                });
            }
        }
    }
}
