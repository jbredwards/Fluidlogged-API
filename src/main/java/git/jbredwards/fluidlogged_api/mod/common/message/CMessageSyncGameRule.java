/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.network.IServerMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import net.minecraft.network.PacketBuffer;
import net.minecraft.world.GameRules;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class CMessageSyncGameRule extends AbstractMessage
{
    public String gameRuleId;
    public boolean updateRender;

    public CMessageSyncGameRule() {}
    public CMessageSyncGameRule(@Nonnull final String gameRuleIdIn, final boolean updateRenderIn) {
        isValid = true;
        gameRuleId = gameRuleIdIn;
        updateRender = updateRenderIn;
    }

    @Override
    public void read(@Nonnull final PacketBuffer buf) {
        gameRuleId = buf.readString(Short.MAX_VALUE);
        updateRender = buf.readBoolean();
    }

    @Override
    public void write(@Nonnull final PacketBuffer buf) {
        buf.writeString(gameRuleId).writeBoolean(updateRender);
    }

    public enum Handler implements IServerMessageHandler<CMessageSyncGameRule, SMessageSyncGameRule>
    {
        INSTANCE;

        @Nonnull
        @Override
        public SMessageSyncGameRule handleMessage(@Nonnull final CMessageSyncGameRule message, @Nonnull final MessageContext ctx) {
            @Nonnull final GameRules gameRules = FMLCommonHandler.instance().getMinecraftServerInstance().getEntityWorld().getGameRules();
            return new SMessageSyncGameRule(message.gameRuleId, gameRules.getString(message.gameRuleId), message.updateRender);
        }
    }
}
