/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.message;

import com.google.gson.JsonObject;
import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.MessageUtils;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class SMessageSyncConfigs extends AbstractMessage
{
    public JsonObject configs;
    public SMessageSyncConfigs() {}
    public SMessageSyncConfigs(@Nonnull final JsonObject configsIn) {
        isValid = true;
        configs = configsIn;
    }

    @Override
    public void read(@Nonnull final PacketBuffer buf) {
        configs = MessageUtils.readJson(buf).getAsJsonObject();
    }

    @Override
    public void write(@Nonnull final PacketBuffer buf) {
        MessageUtils.writeJson(buf, configs);
    }

    public enum Handler implements IClientMessageHandler<SMessageSyncConfigs>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull final SMessageSyncConfigs message, @Nonnull final MessageContext ctx) throws Exception {
            FluidloggedAPIConfigs.init(message.configs);
        }

        @SideOnly(Side.CLIENT)
        @Override
        public boolean isCtxValid(@Nonnull final SMessageSyncConfigs message, @Nonnull final MessageContext ctx) { return true; }
    }
}
