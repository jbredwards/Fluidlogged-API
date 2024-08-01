/*
 * Copyright (c) 2024. jbredwards
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
@Config.LangKey("configgui.fluidloggedAPI.general")
public final class FluidloggedAPIConfig
{
    @Config.LangKey("configgui.fluidloggedAPI.general.allowDefaults")
    public static boolean allowDefaults = true;

    @Nonnull
    @Config.LangKey("configgui.fluidloggedAPI.general.bucketFluidlogging")
    public static BucketFluidloggingMode bucketFluidlogging = BucketFluidloggingMode.ALWAYS;
    public enum BucketFluidloggingMode implements Predicate<Entity>
    {
        ALWAYS("configgui.fluidloggedAPI.general.bucketFluidlogging.always") {
            @Override
            public boolean test(@Nonnull final Entity user) { return true; }
        },
        NO_SNEAK("configgui.fluidloggedAPI.general.bucketFluidlogging.noSneak") {
            @Override
            public boolean test(@Nonnull final Entity user) { return !user.isSneaking(); }
        },
        ON_SNEAK("configgui.fluidloggedAPI.general.bucketFluidlogging.onSneak") {
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
    @Config.LangKey("configgui.fluidloggedAPI.general.downloadModConfigs")
    public static OnlineConfigMode downloadModConfigs = OnlineConfigMode.KEEP_UPDATED;
    public enum OnlineConfigMode
    {
        DISABLED("configgui.fluidloggedAPI.general.downloadModConfigs.disabled"),
        IGNORE_UPDATES("configgui.fluidloggedAPI.general.downloadModConfigs.ignoreUpdates"),
        KEEP_UPDATED("configgui.fluidloggedAPI.general.downloadModConfigs.keepUpdated");

        @Nonnull final String langKey;
        OnlineConfigMode(@Nonnull final String langKeyIn) { langKey = langKeyIn; }

        @Nonnull
        @Override
        public String toString() { return langKey; }
    }

    @Config.LangKey("configgui.fluidloggedAPI.general.ignoreLowFluidCollision")
    public static boolean ignoreLowFluidCollision = true;

    @Nonnull
    @Config.LangKey("configgui.fluidloggedAPI.general.fancyFluidEntityCollision")
    public static FancyCollisionMode fancyFluidEntityCollision = FancyCollisionMode.PLAYERS;
    public enum FancyCollisionMode implements BiPredicate<AxisAlignedBB, Object>
    {
        NEVER("configgui.fluidloggedAPI.general.fancyFluidEntityCollision.never") {
            @Override
            public boolean test(@Nonnull final AxisAlignedBB bb, @Nullable final Object entity) { return false; }
        },
        PLAYERS("configgui.fluidloggedAPI.general.fancyFluidEntityCollision.players") {
            @Override
            public boolean test(@Nonnull final AxisAlignedBB bb, @Nullable final Object entity) { return MEDIUM.test(bb, entity) && entity instanceof EntityPlayer; }
        },
        MEDIUM("configgui.fluidloggedAPI.general.fancyFluidEntityCollision.medium") {
            @Override
            public boolean test(@Nonnull final AxisAlignedBB bb, @Nullable final Object entity) { return bb.maxX - bb.minX < 3 && bb.maxY - bb.minY < 3 && bb.maxZ - bb.minZ < 3; }
        },
        ALWAYS("configgui.fluidloggedAPI.general.fancyFluidEntityCollision.always") {
            @Override
            public boolean test(@Nonnull final AxisAlignedBB bb, @Nullable final Object entity) { return true; }
        };

        @Nonnull final String langKey;
        FancyCollisionMode(@Nonnull final String langKeyIn) { langKey = langKeyIn; }

        @Nonnull
        @Override
        public String toString() { return langKey; }
    }

    @Config.LangKey("configgui.fluidloggedAPI.general.fixBadFluidMixing")
    public static boolean fixBadFluidMixing = true;

    @Config.LangKey("configgui.fluidloggedAPI.general.fluidStateIsFireInsulator")
    public static boolean fluidStateIsFireInsulator = true;

    @Nonnull
    @Config.LangKey("configgui.fluidloggedAPI.general.lavalogVaporizeFlammable")
    public static LavaVaporizingMode lavalogVaporizeFlammable = LavaVaporizingMode.NEVER;
    public enum LavaVaporizingMode
    {
        NEVER("configgui.fluidloggedAPI.general.lavalogVaporizeFlammable.never"),
        FLUIDLOGGABLE("configgui.fluidloggedAPI.general.lavalogVaporizeFlammable.fluidloggable"),
        FLAMMABLE("configgui.fluidloggedAPI.general.lavalogVaporizeFlammable.flammable")/*,
        ALWAYS("configgui.fluidloggedAPI.general.lavalogVaporizeFlammable.always")*/;

        @Nonnull final String langKey;
        LavaVaporizingMode(@Nonnull final String langKeyIn) { langKey = langKeyIn; }

        @Nonnull
        @Override
        public String toString() { return langKey; }
    }

    @Config.LangKey("configgui.fluidloggedAPI.general.nonSourceFluidlogging")
    public static boolean nonSourceFluidlogging = true;
}
