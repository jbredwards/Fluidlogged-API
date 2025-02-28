/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandPrint;
import net.minecraft.client.Minecraft;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.text.TextComponentTranslation;
import net.minecraft.util.text.TextFormatting;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 *
 * @author jbred
 *
 */
public final class SMessageCommandPrint extends AbstractMessage
{
    public Path path;
    public CommandPrint.FluidloggableType fluidloggableType;
    public Object[] args;

    public SMessageCommandPrint() {}
    public SMessageCommandPrint(@Nonnull final Path pathIn, @Nonnull final CommandPrint.FluidloggableType fluidloggableTypeIn, @Nonnull final Object[] argsIn) {
        isValid = true;
        path = pathIn;
        fluidloggableType = fluidloggableTypeIn;
        args = argsIn;
    }

    @Override
    public void read(@Nonnull final PacketBuffer buf) {
        path = Paths.get(buf.readString(Short.MAX_VALUE));
        args = (fluidloggableType = buf.readEnumValue(CommandPrint.FluidloggableType.class)).packetRead(buf);
    }

    @Override
    public void write(@Nonnull final PacketBuffer buf) {
        fluidloggableType.packetWrite(buf.writeString(path.toString()).writeEnumValue(fluidloggableType), args);
    }

    public enum Handler implements IClientMessageHandler<SMessageCommandPrint>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull final SMessageCommandPrint message, @Nonnull final MessageContext ctx) {
            try {
                message.fluidloggableType.save(message.path, message.args);
                Minecraft.getMinecraft().ingameGUI.getChatGUI().printChatMessage(new TextComponentTranslation("commands.fluidlogged_api.generic.finished"));
            }
            catch(@Nonnull final Exception e) {
                Minecraft.getMinecraft().ingameGUI.getChatGUI().printChatMessage(new TextComponentTranslation(TextFormatting.RED + e.getMessage()));
                e.printStackTrace();
            }
        }
    }
}
