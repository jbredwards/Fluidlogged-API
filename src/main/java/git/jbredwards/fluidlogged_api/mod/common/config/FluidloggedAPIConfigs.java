/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.config;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import git.jbredwards.fluidlogged_api.api.event.ReloadFluidloggedAPIEvent;
import git.jbredwards.fluidlogged_api.mod.common.config.handler.*;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Loader;
import net.minecraftforge.fml.common.ModContainer;
import net.minecraftforge.fml.common.ProgressManager;
import org.apache.commons.io.IOUtils;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
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
    /**
     * Loads Fluidlogged API's configs. Note that <i>configs/fluidlogged_api/general.cfg</i> is not handled by this, but rather by forge's config api.
     */
    public static void initConfigs(final boolean isReload) throws IOException {
        @Nonnull Path path;

        // create empty blacklist.cfg file
        path = Paths.get("config", "fluidlogged_api", "blacklist.cfg");
        if(!Files.exists(path)) Files.write(path, Arrays.asList("[", "", "]"), StandardOpenOption.CREATE);

        // create empty fluidTags.cfg file
        path = Paths.get("config", "fluidlogged_api", "fluidTags.cfg");
        if(!Files.exists(path)) Files.write(path, Arrays.asList("[", "", "]"), StandardOpenOption.CREATE);

        // create empty whitelist.cfg file
        path = Paths.get("config", "fluidlogged_api", "whitelist.cfg");
        if(!Files.exists(path)) Files.write(path, Arrays.asList("[", "", "]"), StandardOpenOption.CREATE);

        // initialize config settings from config files
        try {
            FluidTagsConfigHandler.buildDefaults(); // built-in fluid tags
            if(!MinecraftForge.EVENT_BUS.post(new ReloadFluidloggedAPIEvent.Pre(isReload))) {
                LegacyConfigHandler.convertOldFile(); // convert the old config to the new ones if present
                OnlineConfigHandler.downloadModConfigs();

                FluidTagsConfigHandler.init();
                WhitelistConfigHandler.init();
                BlacklistConfigHandler.init();

                MinecraftForge.EVENT_BUS.post(new ReloadFluidloggedAPIEvent.Post(isReload));
            }
        }

        // ensure fluid tag map is always reset, even if an error occurs
        finally { FluidTagsConfigHandler.FLUID_TAGS = null; }
    }

    /**
     * Runs the provided action for each downloaded config file, each builtin config file, and for the user config file.
     */
    public static void forEach(@Nonnull final String fileName, @Nonnull final BiConsumer<String, JsonElement> action) throws IOException {
        @Nonnull final String fileNameRaw = fileName.substring(0, fileName.length() - 4); // fileName without ".cfg" at the end
        @Nonnull final ProgressManager.ProgressBar bar = ProgressManager.push("Configs", Loader.instance().getModList().size() + 1);

        for(@Nonnull final ModContainer mod : Loader.instance().getModList()) {
            bar.step(mod.getName());
            @Nonnull final String fixedModid = mod.getModId().replaceAll("[<>:\"|?*]", "_");

            // for auto config
            if(FluidloggedAPIConfig.downloadModConfigs != FluidloggedAPIConfig.OnlineConfigMode.DISABLED) {
                @Nonnull final Path autoConfig = Paths.get("config/fluidlogged_api/internal", fixedModid, fileName);
                if(Files.exists(autoConfig)) {
                    try { getAsIterable(new JsonParser().parse(Files.newBufferedReader(autoConfig)), Function.identity()).forEach(json -> action.accept(autoConfig.toString(), json)); }
                    catch(@Nonnull final Throwable t) { t.printStackTrace(); } // catch here, to not stop reading other files
                }
            }

            // for mod instances
            if(FluidloggedAPIConfig.allowDefaults) {
                @Nonnull final String path = "/assets/" + fixedModid + "/fluidlogged_api";
                @Nullable InputStream folder = Loader.class.getResourceAsStream(path);
                if(folder != null) { // only proceed if the mod has any fluidlogged api configs
                    IOUtils.closeQuietly(folder);
                    @Nonnull final String file = path + '/' + fileNameRaw;
                    // allow any of the following file types: (cfg, json, jsonc, txt)
                    @Nullable InputStream modConfig = Loader.class.getResourceAsStream(fileName);
                    if(modConfig == null) modConfig = Loader.class.getResourceAsStream(file + ".json");
                    if(modConfig == null) modConfig = Loader.class.getResourceAsStream(file + ".jsonc");
                    if(modConfig == null) modConfig = Loader.class.getResourceAsStream(file + ".txt");
                    if(modConfig != null) {
                        try { getAsIterable(new JsonParser().parse(new BufferedReader(new InputStreamReader(modConfig))), Function.identity()).forEach(json -> action.accept(file, json)); }
                        catch(@Nonnull final Throwable t) { t.printStackTrace(); } // catch here, to not stop reading other files
                    }
                }
            }
        }

        // for user config
        bar.step("User Configs");
        @Nonnull final String file = "config/fluidlogged_api/" + fileName;
        try { getAsIterable(new JsonParser().parse(Files.newBufferedReader(Paths.get(file))), Function.identity()).forEach(json -> action.accept(file, json)); }
        catch(@Nonnull final Throwable t) { t.printStackTrace(); } // catch here to let the game still launch
        ProgressManager.pop(bar);
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
