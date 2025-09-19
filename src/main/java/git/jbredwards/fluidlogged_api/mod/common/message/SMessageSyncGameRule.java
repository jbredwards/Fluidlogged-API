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
import net.minecraft.client.Minecraft;
import net.minecraft.network.PacketBuffer;
import net.minecraft.world.GameRules;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class SMessageSyncGameRule extends AbstractMessage
{
    public String gameRuleId, gameRuleValue;
    public boolean updateRender;

    public SMessageSyncGameRule() {}
    public SMessageSyncGameRule(@Nonnull final String gameRuleIdIn, @Nonnull final String gameRuleValueIn, final boolean updateRenderIn) {
        isValid = true;
        gameRuleId = gameRuleIdIn;
        gameRuleValue = gameRuleValueIn;
        updateRender = updateRenderIn;
    }

    @Override
    public void read(@Nonnull final PacketBuffer buf) {
        gameRuleId = buf.readString(Short.MAX_VALUE);
        gameRuleValue = buf.readString(Short.MAX_VALUE);
        updateRender = buf.readBoolean();
    }

    @Override
    public void write(@Nonnull final PacketBuffer buf) {
        buf.writeString(gameRuleId).writeString(gameRuleValue).writeBoolean(updateRender);
    }

    public enum Handler implements IClientMessageHandler<SMessageSyncGameRule>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull final SMessageSyncGameRule message, @Nonnull final MessageContext ctx) {
            @Nonnull final GameRules gameRules = IClientMessageHandler.getWorldFromContext(ctx).getGameRules();
            if(message.updateRender && !gameRules.getString(message.gameRuleId).equals(message.gameRuleValue)) {
                // Minecraft.getMinecraft().ingameGUI.setOverlayMessage(I18n.format(""), false);
                Minecraft.getMinecraft().renderGlobal.loadRenderers();
            }

            gameRules.setOrCreateGameRule(message.gameRuleId, message.gameRuleValue);
        }
    }
}
