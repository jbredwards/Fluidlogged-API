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

package git.jbredwards.fluidlogged_api.mod.common.config;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import git.jbredwards.fluidlogged_api.api.event.FluidloggedAPIConfigsEvent;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.common.config.handler.*;
import git.jbredwards.fluidlogged_api.mod.common.config.util.ConfigPredicate;
import git.jbredwards.fluidlogged_api.mod.common.message.SMessageSyncConfigs;
import net.minecraft.server.MinecraftServer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Loader;
import net.minecraftforge.fml.common.ModContainer;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Iterator;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 *
 * @author jbred
 *
 */
public final class FluidloggedAPIConfigs
{
    @Nonnull
    public static final Path FOLDER = Paths.get("config", FluidloggedAPI.MODID);

    /**
     * Loads Fluidlogged API's configs, and syncs the data for any connected players.
     * Note that <i>configs/fluidlogged_api/general.cfg</i> is not handled by this, but rather by forge's config api.
     */
    public static void initConfigs(@Nonnull final MinecraftServer server, final boolean isReload) throws IOException {
        try { OnlineConfigHandler.downloadModConfigs(); }
        catch(@Nonnull final IOException e) { FluidloggedAPI.LOGGER.error(e); }

        // process config data
        @Nonnull final JsonObject configs = readConfigFiles(server);
        if(!MinecraftForge.EVENT_BUS.post(new FluidloggedAPIConfigsEvent.Apply(server, configs, isReload))) {
            init(configs);
            if(isReload) { // sync with connected players
                if(server.isDedicatedServer()) FluidloggedAPI.WRAPPER.sendToAll(new SMessageSyncConfigs(configs));
                else if(server.getPlayerList().getCurrentPlayerCount() > 1) server.getPlayerList().getPlayers().forEach(player -> {
                    if(!player.connection.getNetworkManager().isLocalChannel()) FluidloggedAPI.WRAPPER.sendTo(new SMessageSyncConfigs(configs), player);
                });
            }
        }
    }

    /**
     * Reads all config data from disc and returns it as a json object.
     */
    @Nonnull
    public static JsonObject readConfigFiles(@Nonnull final MinecraftServer server) {
        @Nonnull final JsonObject ret = new JsonObject();
        MinecraftForge.EVENT_BUS.post(new FluidloggedAPIConfigsEvent.Read.Pre(server, ret));
        saveToCache(ret, "FLUID_TAGS", "fluidTags");
        saveToCache(ret, "WHITELIST", "whitelist");
        saveToCache(ret, "BLACKLIST", "blacklist");
        MinecraftForge.EVENT_BUS.post(new FluidloggedAPIConfigsEvent.Read.Post(server, ret));
        return ret;
    }

    /**
     * Reads specific config data from disc and saves it to the configs json object.
     */
    private static void saveToCache(@Nonnull final JsonObject configs, @Nonnull final String type, @Nonnull final String fileName) {
        @Nonnull final JsonObject json = new JsonObject(), onlineData = new JsonObject(), modData = new JsonObject();
        configs.add(type, json);

        for(@Nonnull final ModContainer mod : Loader.instance().getModList()) {
            @Nonnull final String fixedModid = mod.getModId().replaceAll("[<>:\"|?*]", "_");

            // for auto config
            if(FluidloggedAPIConfig.downloadModConfigs != FluidloggedAPIConfig.OnlineConfigMode.DISABLED) {
                @Nonnull final Path autoConfig = FOLDER.resolve(Paths.get("internal", fixedModid, fileName + ".jsonc"));
                if(Files.exists(autoConfig)) {
                    try(@Nonnull final Reader reader = Files.newBufferedReader(autoConfig)) { onlineData.add(fixedModid, new JsonParser().parse(reader)); }
                    catch(@Nonnull final Throwable t) { FluidloggedAPI.LOGGER.error("Error occurred while caching " + autoConfig, t); } // catch here, to not stop reading other files
                }
            }

            // for mod instances
            if(FluidloggedAPIConfig.allowDefaults) {
                @Nonnull final String path = "/assets/" + fixedModid + "/fluidlogged_api";
                @Nullable final InputStream folder = Loader.class.getResourceAsStream(path);
                if(folder != null) { // only proceed if the mod has any fluidlogged api configs
                    try { folder.close(); } // NPE may be thrown by old versions of Java 8?
                    catch(@Nonnull final Throwable ignored) {} // assume stream has been closed
                    @Nonnull final String file = path + '/' + fileName;
                    // allow any of the following file types: (cfg, json, jsonc, txt)
                    @Nullable InputStream modConfig = Loader.class.getResourceAsStream(file + ".cfg");
                    if(modConfig == null) modConfig = Loader.class.getResourceAsStream(file + ".json");
                    if(modConfig == null) modConfig = Loader.class.getResourceAsStream(file + ".jsonc");
                    if(modConfig == null) modConfig = Loader.class.getResourceAsStream(file + ".txt");
                    if(modConfig != null) {
                        try(@Nonnull final Reader reader = new BufferedReader(new InputStreamReader(modConfig))) { modData.add(fixedModid, new JsonParser().parse(reader)); }
                        catch(@Nonnull final Throwable t) { FluidloggedAPI.LOGGER.error("Error occurred while caching " + file, t); } // catch here, to not stop reading other files
                    }
                }
            }
        }

        // save builtin configs
        if(onlineData.size() != 0) json.add("ONLINE", onlineData);
        if(modData.size() != 0) json.add("MODDED", modData);

        // for user config
        @Nonnull final Path file = FOLDER.resolve(fileName + ".cfg");
        if(Files.exists(file)) {
            try(@Nonnull final Reader reader = Files.newBufferedReader(file)) { json.add("USER", new JsonParser().parse(reader)); }
            catch(@Nonnull final Throwable t) { FluidloggedAPI.LOGGER.error("Error occurred while caching " + file, t); } // catch here to let the game still launch
        }
    }

    /**
     * Reads fluidlogging data from configs, and resets any existing fluidlogging data.
     */
    public static void init(@Nonnull final JsonObject configs) throws IOException {
        try {
            ConfigPredicate.reset();
            FluidTagsConfigHandler.init(configs);
            WhitelistConfigHandler.init(configs);
            BlacklistConfigHandler.init(configs);
        }

        // ensure fluid tag map is always reset, even if an error occurs
        finally { FluidTagsConfigHandler.FLUID_TAGS = null; }
    }

    /**
     * Runs the provided action for each downloaded config file, each builtin config file, and for the user config file.
     */
    public static void forEach(@Nonnull final JsonObject configs, @Nonnull final String type, @Nonnull final String fileName, @Nonnull final BiConsumer<String, JsonElement> action) throws IOException {
        if(!configs.has(type)) return;
        @Nonnull final JsonObject json = configs.getAsJsonObject(type);

        // for auto configs
        if(json.has("ONLINE")) json.getAsJsonObject("ONLINE").entrySet().forEach(e -> {
            @Nonnull final String file = FOLDER + "/internal/" + e.getKey() + '/' + fileName;
            try { getAsIterable(e.getValue(), Function.identity()).forEach(element -> action.accept(file, element)); }
            catch(@Nonnull final Throwable t) { FluidloggedAPI.LOGGER.error("Error occurred while interpreting " + file, t); } // catch here, to not stop reading other files
        });

        // for mod configs
        if(json.has("MODDED")) json.getAsJsonObject("MODDED").entrySet().forEach(e -> {
            @Nonnull final String file = "/assets/" + e.getKey() + "/fluidlogged_api/" + fileName;
            try { getAsIterable(e.getValue(), Function.identity()).forEach(element -> action.accept(file, element)); }
            catch(@Nonnull final Throwable t) { FluidloggedAPI.LOGGER.error("Error occurred while interpreting " + file, t); } // catch here, to not stop reading other files
        });

        // for user config
        if(json.has("USER")) {
            @Nonnull final String file = FOLDER.toString() + '/' + fileName + ".cfg";
            try { getAsIterable(json.get("USER"), Function.identity()).forEach(element -> action.accept(file, element)); }
            catch(@Nonnull final Throwable t) { FluidloggedAPI.LOGGER.error("Error occurred while interpreting " + file, t); } // catch here to let the game still launch
        }
    }

    /**
     * Utility function that returns the provided json as an ordered iterable.
     */
    @Nonnull
    public static <T> Iterable<T> getAsIterable(@Nonnull final JsonElement json, @Nonnull final Function<JsonElement, T> mapper) {
        return json.isJsonArray() ? () -> new Iterator<T>() {
            @Nonnull
            final Iterator<JsonElement> it = json.getAsJsonArray().iterator();

            @Nonnull
            @Override
            public T next() { return mapper.apply(it.next()); }

            @Override
            public boolean hasNext() { return it.hasNext(); }

            @Override
            public void remove() { it.remove(); }
        } : Collections.singleton(mapper.apply(json));
    }
}
