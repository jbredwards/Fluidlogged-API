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
import java.util.function.UnaryOperator;

/**
 *
 * @author jbred
 *
 */
public final class WhitelistConfigHandler
{
    @Nonnull
    private static final String error = "An error occurred while parsing a whitelist entry in file \"%s\", skipping...";
    public static void init() throws IOException {
        IConfigAccessor.WHITELIST_CACHE.forEach(state -> { state.setWhitelistPredicate(null); ICanFluidFlowHandler.Accessor.setOverride(state, null); });
        IConfigAccessor.WHITELIST_CACHE.clear();

        // run for auto configs, mod instances, and user config
        FluidloggedAPIConfigs.forEach("whitelist.cfg", (file, jsonIn) -> {
            try { ConfigPredicate.deserialize(file, jsonIn.getAsJsonObject(), IConfigAccessor::getWhitelistPredicate, IConfigAccessor::setWhitelistAndCache, UnaryOperator.identity()); }
            catch(@Nonnull final Throwable t) { new JsonParseException(String.format(error, file), t).printStackTrace(); }
        });
    }
}
