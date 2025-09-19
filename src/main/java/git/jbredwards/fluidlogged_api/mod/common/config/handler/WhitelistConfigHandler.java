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

package git.jbredwards.fluidlogged_api.mod.common.config.handler;

import com.google.gson.JsonObject;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
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
    public static void init(@Nonnull final JsonObject configs) throws IOException {
        // run for auto configs, mod instances, and user config
        FluidloggedAPIConfigs.forEach(configs, "WHITELIST", "whitelist", (file, jsonIn) -> {
            try { ConfigPredicate.deserialize(file, jsonIn.getAsJsonObject(), IConfigAccessor::getWhitelistPredicate, IConfigAccessor::setWhitelistAndCache, UnaryOperator.identity()); }
            catch(@Nonnull final Throwable t) { FluidloggedAPI.LOGGER.error(String.format(error, file), t); }
        });
    }
}
