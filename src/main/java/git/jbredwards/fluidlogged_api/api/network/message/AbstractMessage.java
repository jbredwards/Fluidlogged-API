package git.jbredwards.fluidlogged_api.api.network.message;

import io.netty.buffer.ByteBuf;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public abstract class AbstractMessage implements IMessage
{
    public boolean isValid;

    @Override
    public final void fromBytes(@Nonnull ByteBuf buf) {
        isValid = buf.readBoolean();
        if(isValid) read(new PacketBuffer(buf));
    }

    @Override
    public final void toBytes(@Nonnull ByteBuf buf) {
        buf.writeBoolean(isValid);
        if(isValid) write(new PacketBuffer(buf));
    }

    public abstract void read(@Nonnull PacketBuffer buf);
    public abstract void write(@Nonnull PacketBuffer buf);
}
