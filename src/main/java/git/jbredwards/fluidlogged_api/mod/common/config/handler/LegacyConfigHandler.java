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
import com.google.gson.JsonParser;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import net.minecraft.util.JsonUtils;
import net.minecraftforge.common.config.Config;
import net.minecraftforge.common.config.ConfigManager;
import net.minecraftforge.common.crafting.CraftingHelper;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.*;
import java.util.Arrays;
import java.util.Collections;

/**
 *
 * @author jbred
 *
 */
public final class LegacyConfigHandler
{
    @Nonnull
    private static final Path OLD_CONFIG_PATH = Paths.get("config", "fluidlogged_api.cfg");
    public static void convertOldFile() throws IOException {
        @Nonnull final Path blacklist = FluidloggedAPIConfigs.FOLDER.resolve("blacklist.cfg");
        @Nonnull final Path fluidTags = FluidloggedAPIConfigs.FOLDER.resolve("fluidTags.cfg");
        @Nonnull final Path whitelist = FluidloggedAPIConfigs.FOLDER.resolve("whitelist.cfg");

        if(!Files.exists(blacklist)) Files.write(blacklist, Collections.singleton("[\n\n]"), StandardOpenOption.CREATE);
        if(!Files.exists(fluidTags)) Files.write(fluidTags, Collections.singleton("[\n\n]"), StandardOpenOption.CREATE);
        if(!Files.exists(whitelist)) Files.write(whitelist, Collections.singleton("[\n\n]"), StandardOpenOption.CREATE);

        // convert old file
        if(Files.exists(OLD_CONFIG_PATH)) {
            @Nonnull final byte[] bytes = Files.readAllBytes(OLD_CONFIG_PATH);
            @Nonnull final JsonObject json = new JsonParser().parse('{' + new String(bytes) + '}').getAsJsonObject();

            // disable certain new settings, to preserve legacy functionality
            FluidloggedAPIConfig.bucketFluidlogging = FluidloggedAPIConfig.BucketFluidloggingMode.NO_SNEAK;
            FluidloggedAPIConfig.downloadModConfigs = FluidloggedAPIConfig.OnlineConfigMode.DISABLED;
            FluidloggedAPIConfig.fancyFluidEntityCollision = FluidloggedAPIConfig.FancyCollisionMode.NEVER;
            FluidloggedAPIConfig.lavalogVaporizeFlammable = FluidloggedAPIConfig.LavaVaporizingMode.NEVER;
            FluidloggedAPIConfig.ignoreLowFluidCollision = false;
            FluidloggedAPIConfig.fluidStateIsFireInsulator = false;
            FluidloggedAPIConfig.nonSourceFluidlogging = false;

            // move generic settings
            if(json.has("applyDefaults") && !JsonUtils.getBoolean(json.get("applyDefaults"), "applyDefaults")) FluidloggedAPIConfig.allowDefaults = false;
            if(json.has("fancyFluidEntityCollision") && JsonUtils.getBoolean(json.get("fancyFluidEntityCollision"), "fancyFluidEntityCollision")) FluidloggedAPIConfig.fancyFluidEntityCollision = FluidloggedAPIConfig.FancyCollisionMode.ALWAYS;
            if(json.has("lavalogVaporizeFlammable") && JsonUtils.getBoolean(json.get("lavalogVaporizeFlammable"), "lavalogVaporizeFlammable")) FluidloggedAPIConfig.lavalogVaporizeFlammable = FluidloggedAPIConfig.LavaVaporizingMode.FLUIDLOGGABLE;
            ConfigManager.sync(FluidloggedAPI.MODID, Config.Type.INSTANCE);

            // move specialized settings
            if(json.has("blacklist")) try(@Nonnull final Writer writer = Files.newBufferedWriter(blacklist)) { CraftingHelper.GSON.toJson(json.get("blacklist"), writer); }
            if(json.has("fluidTags")) try(@Nonnull final Writer writer = Files.newBufferedWriter(fluidTags)) { CraftingHelper.GSON.toJson(json.get("fluidTags"), writer); }
            if(json.has("whitelist")) try(@Nonnull final Writer writer = Files.newBufferedWriter(whitelist)) { CraftingHelper.GSON.toJson(json.get("whitelist"), writer); }

            // move old settings to a backup file, then delete the old file
            Files.write(Paths.get("config", "fluidlogged_api.cfg_old"), bytes);
            Files.delete(OLD_CONFIG_PATH);

            // create readme file to tell users what happened to their old config
            Files.write(Paths.get("config", "fluidlogged_api.cfg_README.txt"), Arrays.asList(
                    "As of Fluidlogged API v3.0.0, the config is broken up into multiple files, and is stored in a new `./config/fluidlogged_api` folder.",
                    "",
                    "Any data in your old config should have been converted automatically. But just in case, a copy of your old config exists as `./config/fluidlogged_api.cfg_old`.",
                    "WARNING: Do NOT change the suffix of the old config file back to `.cfg`! This will make fluidlogged api try to convert the file again, voiding any changes you may have made to the new config files since the previous conversion!",
                    "",
                    "Once you've made sure all of your old settings have been transferred correctly, feel free to delete this README file and `./config/fluidlogged_api.cfg_old`!"
            ));
        }
    }
}
