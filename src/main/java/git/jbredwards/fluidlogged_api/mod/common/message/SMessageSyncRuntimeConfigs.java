/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.message;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import io.netty.buffer.ByteBufInputStream;
import io.netty.buffer.ByteBufOutputStream;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.io.IOUtils;

import javax.annotation.Nonnull;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;

/**
 *
 * @author jbred
 *
 */
public final class SMessageSyncRuntimeConfigs extends AbstractMessage
{
    @Nonnull
    private static final Gson GSON = new Gson();

    public JsonObject configs;
    public SMessageSyncRuntimeConfigs() {}
    public SMessageSyncRuntimeConfigs(@Nonnull final JsonObject configsIn) {
        isValid = true;
        configs = configsIn;
    }

    @Override
    public void read(@Nonnull final PacketBuffer buf) {
        @Nonnull final InputStreamReader reader = new InputStreamReader(new ByteBufInputStream(buf, buf.readInt()), StandardCharsets.UTF_8);
        configs = new JsonParser().parse(reader).getAsJsonObject();
        IOUtils.closeQuietly(reader);
    }

    @Override
    public void write(@Nonnull final PacketBuffer buf) {
        buf.writeInt(0); // allocate size bytes
        final int startIndex = buf.writerIndex();

        @Nonnull final OutputStreamWriter writer = new OutputStreamWriter(new ByteBufOutputStream(buf), StandardCharsets.UTF_8);
        GSON.toJson(configs, writer);
        IOUtils.closeQuietly(writer);

        buf.setInt(startIndex - 4, buf.writerIndex() - startIndex); // write size to allocated bytes
    }

    public enum Handler implements IClientMessageHandler<SMessageSyncRuntimeConfigs>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull final SMessageSyncRuntimeConfigs message, @Nonnull final MessageContext ctx) throws Exception {
            FluidloggedAPIConfigs.init(message.configs);
        }
    }
}
