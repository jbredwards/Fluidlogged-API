/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.config.handler;

import com.google.gson.JsonParseException;
import git.jbredwards.fluidlogged_api.mod.asm.iface.ICanFluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigAccessor;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import git.jbredwards.fluidlogged_api.mod.common.config.util.ConfigPredicate;

import javax.annotation.Nonnull;
import java.io.IOException;

/**
 *
 * @author jbred
 *
 */
public final class BlacklistConfigHandler
{
    @Nonnull
    private static final String error = "An error occurred while parsing a blacklist entry in file \"%s\", skipping...";
    public static void init() throws IOException {
        IConfigAccessor.BLACKLIST_CACHE.forEach(state -> { state.setBlacklistPredicate(null); ICanFluidFlowHandler.Accessor.setOverride(state, null); });
        IConfigAccessor.BLACKLIST_CACHE.clear();

        // run for auto configs, mod instances, and user config
        FluidloggedAPIConfigs.forEach("blacklist.cfg", (file, jsonIn) -> {
            try { ConfigPredicate.deserialize(file, jsonIn.getAsJsonObject(), IConfigAccessor::getBlacklistPredicate, IConfigAccessor::setBlacklistAndCache, ICanFluidFlowHandler::negate); }
            catch(@Nonnull final Throwable t) { new JsonParseException(String.format(error, file), t).printStackTrace(); }
        });
    }
}
