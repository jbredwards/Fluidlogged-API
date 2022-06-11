package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import io.netty.buffer.ByteBuf;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;

/**
 *
 * @author jbred
 *
 */
public class ReloadCfgMessage implements IMessage
{
    @Override
    public void fromBytes(@Nonnull ByteBuf buf) { }

    @Override
    public void toBytes(@Nonnull ByteBuf buf) { }

    public enum Handler implements IMessageHandler<ReloadCfgMessage, IMessage>
    {
        INSTANCE;

        @Nullable
        @Override
        public IMessage onMessage(@Nonnull ReloadCfgMessage message, @Nonnull MessageContext ctx) {
            try {
                ConfigHandler.init();
                ConfigHandler.complete();
            }
            //oops
            catch(IOException ignored) { }
            return null;
        }
    }
}
