package git.jbredwards.fluidlogged_api.api.network.message;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import io.netty.buffer.ByteBuf;
import net.minecraft.client.Minecraft;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * send a FluidState to the client
 * @author jbred
 *
 */
public final class MessageFluidState extends AbstractMessage
{
    public long pos;
    public int state;
    public boolean doRenderUpdate;

    public MessageFluidState() {}
    public MessageFluidState(@Nonnull BlockPos pos, @Nonnull FluidState state, boolean doRenderUpdate) {
        this.isValid = true;
        this.pos = pos.toLong();
        this.state = state.serialize();
        this.doRenderUpdate = doRenderUpdate;
    }

    @Override
    public void read(@Nonnull ByteBuf buf) {
        pos = buf.readLong();
        state = buf.readInt();
        doRenderUpdate = buf.readBoolean();
    }

    @Override
    public void write(@Nonnull ByteBuf buf) {
        buf.writeLong(pos);
        buf.writeInt(state);
        buf.writeBoolean(doRenderUpdate);
    }

    public enum Handler implements IClientMessageHandler<MessageFluidState>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull MessageFluidState message) {
            final World world = Minecraft.getMinecraft().world;
            final BlockPos pos = BlockPos.fromLong(message.pos);
            final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(world.getChunk(pos));

            if(cap != null) {
                final FluidState fluidState = FluidState.deserialize(message.state);
                //send changes to client
                cap.getContainer(pos).setFluidState(pos, fluidState);
                //re-render block
                if(message.doRenderUpdate) world.markBlockRangeForRenderUpdate(pos, pos);
                FluidloggedUtils.relightFluidBlock(world, pos, fluidState);
            }
        }
    }
}
