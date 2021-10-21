package git.jbredwards.fluidlogged_api.common.network;

import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import io.netty.buffer.ByteBuf;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
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
    public boolean isValid = false;
    public BlockPos pos;
    public IBlockState state;

    @SuppressWarnings("unused")
    public FluidStateMessage() {}
    public FluidStateMessage(@Nonnull BlockPos pos, @Nullable IBlockState state) {
        this.isValid = true;
        this.pos = pos;
        this.state = state;
    }

    @Override
    public void fromBytes(@Nonnull ByteBuf buf) {
        isValid = buf.readBoolean();
        if(isValid) {
            pos = BlockPos.fromLong(buf.readLong());
            int stateId = buf.readInt();
            state = stateId < 0 ? null : Block.getStateById(stateId);
        }
    }

    @Override
    public void toBytes(@Nonnull ByteBuf buf) {
        buf.writeBoolean(isValid);
        if(isValid) {
            buf.writeLong(pos.toLong());
            buf.writeInt(state == null ? -1 : Block.getStateId(state));
        }
    }

    public enum Handler implements IMessageHandler<FluidStateMessage, IMessage>
    {
        INSTANCE;

        @Nullable
        @Override
        public IMessage onMessage(@Nonnull FluidStateMessage m, @Nonnull MessageContext ctx) {
            if(m.isValid && ctx.side == Side.CLIENT) {
                Minecraft.getMinecraft().addScheduledTask(() -> {
                    final WorldClient world = Minecraft.getMinecraft().world;
                    final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(world.getChunkFromBlockCoords(m.pos));

                    if(cap != null) {
                        cap.setFluidState(m.pos, m.state);
                        world.markBlockRangeForRenderUpdate(m.pos, m.pos);
                    }
                });
            }
            //no return packet
            return null;
        }
    }
}
