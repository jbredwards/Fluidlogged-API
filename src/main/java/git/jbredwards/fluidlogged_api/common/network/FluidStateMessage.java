package git.jbredwards.fluidlogged_api.common.network;

import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import io.netty.buffer.ByteBuf;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public final class FluidStateMessage implements IMessage
{
    public boolean isValid;
    public long pos;
    public int state;

    @SuppressWarnings("unused")
    public FluidStateMessage() {}
    public FluidStateMessage(@Nonnull BlockPos pos, @Nonnull FluidState state) {
        this.isValid = true;
        this.pos = pos.toLong();
        this.state = state.serialize();
    }

    @Override
    public void fromBytes(@Nonnull ByteBuf buf) {
        isValid = buf.readBoolean();
        if(isValid) {
            pos = buf.readLong();
            state = buf.readInt();
        }
    }

    @Override
    public void toBytes(@Nonnull ByteBuf buf) {
        buf.writeBoolean(isValid);
        if(isValid) {
            buf.writeLong(pos);
            buf.writeInt(state);
        }
    }

    public enum Handler implements IMessageHandler<FluidStateMessage, IMessage>
    {
        INSTANCE;

        @Nullable
        @Override
        public IMessage onMessage(@Nonnull FluidStateMessage message, @Nonnull MessageContext ctx) {
            if(message.isValid && ctx.side == Side.CLIENT) {
                Minecraft.getMinecraft().addScheduledTask(() -> {
                    final WorldClient world = Minecraft.getMinecraft().world;
                    final BlockPos pos = BlockPos.fromLong(message.pos);

                    final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(
                            world.getChunkFromBlockCoords(pos));

                    if(cap != null) {
                        //send changes to client
                        cap.setFluidState(pos, FluidState.deserialize(message.state));

                        //update light levels
                        world.profiler.startSection("checkLight");
                        world.checkLight(pos);
                        world.profiler.endSection();

                        //re-render block
                        world.markBlockRangeForRenderUpdate(pos, pos);
                    }
                });
            }
            //no return packet
            return null;
        }
    }
}
