/*
 * Copyright (c) 2024-2025. jbredwards
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
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
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
            readmeURL = "https://raw.githubusercontent.com/jbredwards/Fluidlogged-API-Configs/refs/heads/1.12.2/internal/README.txt",
            versionsURL = "https://raw.githubusercontent.com/jbredwards/Fluidlogged-API-Configs/refs/heads/1.12.2/internal/versions.jsonc",
            autoConfigURL = "https://raw.githubusercontent.com/jbredwards/Fluidlogged-API-Configs/refs/heads/1.12.2/internal/%s/%s";

    public static void downloadModConfigs() throws IOException {
        if(FluidloggedAPIConfig.downloadModConfigs != FluidloggedAPIConfig.OnlineConfigMode.KEEP_UPDATED) return;
        else try(@Nonnull final InputStream stream = new URL(readmeURL).openStream()) { createFile(readmePath, stream); }

        // save old versions.jsonc, to compare to the newly downloaded versions.jsonc
        @Nonnull final JsonObject oldVersions;
        if(!Files.exists(versionsPath)) oldVersions = new JsonObject();
        else try(@Nonnull final Reader reader = Files.newBufferedReader(versionsPath)) { oldVersions = new JsonParser().parse(reader).getAsJsonObject(); }

        // write new versions.jsonc file
        @Nonnull final JsonObject newVersions;
        try(@Nonnull final InputStream stream = new URL(versionsURL).openStream()) { createFile(versionsPath, stream); }
        try(@Nonnull final Reader reader = Files.newBufferedReader(versionsPath)) { newVersions = new JsonParser().parse(reader).getAsJsonObject(); }

        // update auto mod configs cache if needed
        for(@Nonnull final ModContainer mod : Loader.instance().getModList()) {
            @Nonnull final String modid = mod.getModId().replaceAll("[<>:\"|?*]", "_");
            @Nullable final JsonElement newElement = newVersions.get(modid);
            @Nullable final JsonElement oldElement = oldVersions.get(modid);

            if(newElement == null) { if(oldElement != null) FileUtils.deleteDirectory(new File("config/fluidlogged_api/internal", modid)); }
            else if(oldElement == null || JsonUtils.getInt(oldElement, modid) < JsonUtils.getInt(newElement, modid)) {

                downloadModConfig(modid, "whitelist.jsonc");
                downloadModConfig(modid, "blacklist.jsonc");
                downloadModConfig(modid, "fluidTags.jsonc");
            }
        }
    }

    private static void downloadModConfig(@Nonnull final String modid, @Nonnull final String config) throws IOException {
        @Nullable InputStream stream = null;
        try { stream = new URL(String.format(autoConfigURL, modid, config)).openStream(); }
        catch(@Nonnull final Throwable ignored) {} // mod does not have an auto config, skip

        @Nonnull final Path path = Paths.get("config/fluidlogged_api/internal", modid, config);
        if(stream == null) Files.deleteIfExists(path); // delete any old configs that have been removed from the repo
        else { // download any new configs that have been added or updated in the repo
            try { createFile(path, stream); }
            finally { IOUtils.closeQuietly(stream); }
        }
    }

    private static void createFile(@Nonnull final Path path, @Nonnull final InputStream data) throws IOException {
        if(!Files.exists(path)) {
            Files.createDirectories(path.getParent());
            Files.createFile(path);
        }

        Files.copy(data, path, StandardCopyOption.REPLACE_EXISTING);
    }
}
