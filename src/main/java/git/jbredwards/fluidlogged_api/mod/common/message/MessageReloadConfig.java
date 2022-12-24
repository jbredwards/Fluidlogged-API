package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigHandler;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import java.io.IOException;

/**
 *
 * @author jbred
 *
 */
public class MessageReloadConfig extends AbstractMessage
{
    public MessageReloadConfig() {}
    public MessageReloadConfig(boolean isValid) { this.isValid = isValid; }

    @Override
    public void read(@Nonnull PacketBuffer buf) {}

    @Override
    public void write(@Nonnull PacketBuffer buf) {}

    public enum Handler implements IClientMessageHandler<MessageReloadConfig>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull MessageReloadConfig message) {
            try {
                FluidloggedAPIConfigHandler.init();
                FluidloggedAPIConfigHandler.complete();
            }

            //oops
            catch(IOException ignored) {}
        }
    }
}
