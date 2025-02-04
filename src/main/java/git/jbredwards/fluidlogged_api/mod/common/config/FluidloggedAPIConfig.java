/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.config;

import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraftforge.common.config.Config;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.function.BiPredicate;
import java.util.function.Predicate;

/**
 * Stores fluidlogged api's general config settings
 * @author jbred
 *
 */
@Config(modid = FluidloggedAPI.MODID, name = "fluidlogged_api/general")
@Config.LangKey("configgui.fluidlogged_api.general")
public final class FluidloggedAPIConfig
{
    @Config.RequiresWorldRestart // prevent multiplayer desync
    @Config.LangKey("configgui.fluidlogged_api.general.allowDefaults")
    public static boolean allowDefaults = true;

    @Nonnull
    @Config.LangKey("configgui.fluidlogged_api.general.bucketFluidlogging")
    public static BucketFluidloggingMode bucketFluidlogging = BucketFluidloggingMode.ALWAYS;
    public enum BucketFluidloggingMode implements Predicate<Entity>
    {
        ALWAYS("configgui.fluidlogged_api.general.bucketFluidlogging.ALWAYS") {
            @Override
            public boolean test(@Nonnull final Entity user) { return true; }
        },
        NO_SNEAK("configgui.fluidlogged_api.general.bucketFluidlogging.NO_SNEAK") {
            @Override
            public boolean test(@Nonnull final Entity user) { return !user.isSneaking(); }
        },
        ON_SNEAK("configgui.fluidlogged_api.general.bucketFluidlogging.ON_SNEAK") {
            @Override
            public boolean test(@Nonnull final Entity user) { return user.isSneaking(); }
        };

        @Nonnull final String langKey;
        BucketFluidloggingMode(@Nonnull final String langKeyIn) { langKey = langKeyIn; }

        @Nonnull
        @Override
        public String toString() { return langKey; }
    }

    @Nonnull
    @Config.RequiresWorldRestart // prevent multiplayer desync
    @Config.LangKey("configgui.fluidlogged_api.general.downloadModConfigs")
    public static OnlineConfigMode downloadModConfigs = OnlineConfigMode.KEEP_UPDATED;
    public enum OnlineConfigMode
    {
        DISABLED("configgui.fluidlogged_api.general.downloadModConfigs.DISABLED"),
        IGNORE_UPDATES("configgui.fluidlogged_api.general.downloadModConfigs.IGNORE_UPDATES"),
        KEEP_UPDATED("configgui.fluidlogged_api.general.downloadModConfigs.KEEP_UPDATED");

        @Nonnull final String langKey;
        OnlineConfigMode(@Nonnull final String langKeyIn) { langKey = langKeyIn; }

        @Nonnull
        @Override
        public String toString() { return langKey; }
    }

    @Config.LangKey("configgui.fluidlogged_api.general.ignoreLowFluidCollision")
    public static boolean ignoreLowFluidCollision = true;

    @Nonnull
    @Config.LangKey("configgui.fluidlogged_api.general.fancyFluidEntityCollision")
    public static FancyCollisionMode fancyFluidEntityCollision = FancyCollisionMode.PLAYERS;
    public enum FancyCollisionMode implements BiPredicate<AxisAlignedBB, Object>
    {
        NEVER("configgui.fluidlogged_api.general.fancyFluidEntityCollision.NEVER") {
            @Override
            public boolean test(@Nonnull final AxisAlignedBB bb, @Nullable final Object entity) { return false; }
        },
        PLAYERS("configgui.fluidlogged_api.general.fancyFluidEntityCollision.PLAYERS") {
            @Override
            public boolean test(@Nonnull final AxisAlignedBB bb, @Nullable final Object entity) { return MEDIUM.test(bb, entity) && entity instanceof EntityPlayer; }
        },
        MEDIUM("configgui.fluidlogged_api.general.fancyFluidEntityCollision.MEDIUM") {
            @Override
            public boolean test(@Nonnull final AxisAlignedBB bb, @Nullable final Object entity) { return bb.maxX - bb.minX < 3 && bb.maxY - bb.minY < 3 && bb.maxZ - bb.minZ < 3; }
        },
        ALWAYS("configgui.fluidlogged_api.general.fancyFluidEntityCollision.ALWAYS") {
            @Override
            public boolean test(@Nonnull final AxisAlignedBB bb, @Nullable final Object entity) { return true; }
        };

        @Nonnull final String langKey;
        FancyCollisionMode(@Nonnull final String langKeyIn) { langKey = langKeyIn; }

        @Nonnull
        @Override
        public String toString() { return langKey; }
    }

    @Config.LangKey("configgui.fluidlogged_api.general.fancyFluidRenderer")
    public static boolean fancyFluidRenderer = true;

    @Config.LangKey("configgui.fluidlogged_api.general.fixBadFluidMixing")
    public static boolean fixBadFluidMixing = true;

    @Config.LangKey("configgui.fluidlogged_api.general.fluidStateIsFireInsulator")
    public static boolean fluidStateIsFireInsulator = true;

    @Nonnull
    @Config.LangKey("configgui.fluidlogged_api.general.lavalogVaporizeFlammable")
    public static LavaVaporizingMode lavalogVaporizeFlammable = LavaVaporizingMode.NEVER;
    public enum LavaVaporizingMode
    {
        NEVER("configgui.fluidlogged_api.general.lavalogVaporizeFlammable.NEVER"),
        FLUIDLOGGABLE("configgui.fluidlogged_api.general.lavalogVaporizeFlammable.FLUIDLOGGABLE"),
        FLAMMABLE("configgui.fluidlogged_api.general.lavalogVaporizeFlammable.FLAMMABLE");

        @Nonnull final String langKey;
        LavaVaporizingMode(@Nonnull final String langKeyIn) { langKey = langKeyIn; }

        @Nonnull
        @Override
        public String toString() { return langKey; }
    }

    @Config.LangKey("configgui.fluidlogged_api.general.nonSourceFluidlogging")
    public static boolean nonSourceFluidlogging = true;
}
