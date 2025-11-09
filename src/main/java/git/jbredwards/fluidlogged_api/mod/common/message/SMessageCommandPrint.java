/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
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
import javax.annotation.Nullable;
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
    public SMessageCommandPrint(@Nullable final Path pathIn, @Nonnull final CommandPrint.FluidloggableType fluidloggableTypeIn, @Nonnull final Object[] argsIn) {
        isValid = true;
        path = pathIn;
        fluidloggableType = fluidloggableTypeIn;
        args = argsIn;
    }

    @Override
    public void read(@Nonnull final PacketBuffer buf) {
        path = buf.readBoolean() ? Paths.get(buf.readString(Short.MAX_VALUE)) : CommandPrint.getFallbackDirectory();
        args = (fluidloggableType = buf.readEnumValue(CommandPrint.FluidloggableType.class)).packetRead(buf);
    }

    @Override
    public void write(@Nonnull final PacketBuffer buf) {
        buf.writeBoolean(path != null);
        if(path != null) buf.writeString(path.toString());
        fluidloggableType.packetWrite(buf.writeEnumValue(fluidloggableType), args);
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
