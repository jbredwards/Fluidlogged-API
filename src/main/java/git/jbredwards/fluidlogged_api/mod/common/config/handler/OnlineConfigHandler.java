/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.config.handler;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.util.JsonUtils;
import net.minecraftforge.fml.common.Loader;
import net.minecraftforge.fml.common.ModContainer;
import net.minecraftforge.fml.common.ProgressManager;
import org.apache.commons.io.FileUtils;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

/**
 * <a href="https://github.com/jbredwards/Fluidlogged-API-Configs">https://github.com/jbredwards/Fluidlogged-API-Configs</a>
 * @author jbred
 *
 */
public final class OnlineConfigHandler
{
    @Nonnull static final Path
            readmePath = Paths.get("config/fluidlogged_api/internal/README.txt"),
            versionsPath = Paths.get("config/fluidlogged_api/internal/versions.jsonc");
    @Nonnull static final String
            readmeURL = "https://raw.githubusercontent.com/jbredwards/Fluidlogged-API-Configs/1.12.2/internal/README.txt",
            versionsURL = "https://raw.githubusercontent.com/jbredwards/Fluidlogged-API-Configs/1.12.2/internal/versions.jsonc",
            autoConfigURL = "https://raw.githubusercontent.com/jbredwards/Fluidlogged-API-Configs/1.12.2/internal/%s/%s";

    public static void downloadModConfigs() throws IOException {
        if(FluidloggedAPIConfig.downloadModConfigs != FluidloggedAPIConfig.OnlineConfigMode.KEEP_UPDATED) return;
        else if(new InetSocketAddress("https://raw.githubusercontent.com", 0).isUnresolved()) return; // no internet connection

        @Nonnull final ProgressManager.ProgressBar bar = ProgressManager.push("Online Configs", Loader.instance().getModList().size() + 2);
        @Nullable InputStream stream;

        bar.step("File: README.txt");
        stream = new URL(readmeURL).openStream();
        Files.copy(stream, readmePath, StandardCopyOption.REPLACE_EXISTING);

        bar.step("File: versions.jsonc");
        stream = new URL(versionsURL).openStream();

        // compare to old versions.jsonc to the newly downloaded versions.jsonc
        @Nonnull final JsonObject oldVersions = Files.exists(versionsPath) ? new JsonParser().parse(Files.newBufferedReader(versionsPath)).getAsJsonObject() : new JsonObject();
        Files.copy(stream, versionsPath, StandardCopyOption.REPLACE_EXISTING); // store new versions.jsonc file
        @Nonnull final JsonObject newVersions = new JsonParser().parse(Files.newBufferedReader(versionsPath)).getAsJsonObject();

        // update auto mod configs cache if needed
        @Nonnull Path path;
        for(@Nonnull final ModContainer mod : Loader.instance().getModList()) {
            bar.step("Mod: " + mod.getName());

            @Nonnull final String modid = mod.getModId();
            @Nullable final JsonElement newElement = newVersions.get(modid);
            @Nullable final JsonElement oldElement = oldVersions.get(modid);

            if(newElement == null) { if(oldElement != null) FileUtils.deleteDirectory(new File("config/fluidlogged_api/internal", modid)); }
            else if(oldElement == null || JsonUtils.getInt(oldElement, modid) < JsonUtils.getInt(newElement, modid)) {

                // whitelist
                stream = null;
                try { stream = new URL(String.format(autoConfigURL, modid, "whitelist.jsonc")).openStream(); }
                catch(@Nonnull final Throwable ignored) {} // mod does not have an auto whitelist config, skip
                path = Paths.get("config/fluidlogged_api/internal", modid, "whitelist.jsonc");
                if(stream != null) Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING);
                else Files.delete(path); // delete any old configs that have been removed from the repo

                // blacklist
                stream = null;
                try { stream = new URL(String.format(autoConfigURL, modid, "blacklist.jsonc")).openStream(); }
                catch(@Nonnull final Throwable ignored) {} // mod does not have an auto blacklist config, skip
                path = Paths.get("config/fluidlogged_api/internal", modid, "blacklist.jsonc");
                if(stream != null) Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING);
                else Files.delete(path); // delete any old configs that have been removed from the repo

                // fluid tags
                stream = null;
                try { stream = new URL(String.format(autoConfigURL, modid, "fluidTags.jsonc")).openStream(); }
                catch(@Nonnull final Throwable ignored) {} // mod does not have an auto fluid tag config, skip
                path = Paths.get("config/fluidlogged_api/internal", modid, "fluidTags.jsonc");
                if(stream != null) Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING);
                else Files.delete(path); // delete any old configs that have been removed from the repo
            }
        }

        ProgressManager.pop(bar);
    }
}
