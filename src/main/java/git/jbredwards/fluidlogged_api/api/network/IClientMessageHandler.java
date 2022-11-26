package git.jbredwards.fluidlogged_api.api.network;

import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import net.minecraft.client.Minecraft;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public interface IClientMessageHandler<REQ extends AbstractMessage> extends IMessageHandler<REQ, IMessage>
{
    @Nullable
    @Override
    default IMessage onMessage(@Nonnull REQ message, @Nonnull MessageContext ctx) {
        if(message.isValid && ctx.side.isClient())
            Minecraft.getMinecraft().addScheduledTask(() -> handleMessage(message));

        return null;
    }

    @SideOnly(Side.CLIENT)
    void handleMessage(@Nonnull REQ message);
}
