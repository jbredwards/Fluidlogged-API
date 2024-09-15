/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.event;

import com.google.gson.JsonObject;
import net.minecraft.server.MinecraftServer;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;

/**
 *
 * @since 3.0.0
 * @author jbred
 *
 */
@Cancelable
public abstract class FluidloggedAPIConfigsEvent extends Event
{
    @Nonnull public final MinecraftServer server;
    @Nonnull public final JsonObject configs;
    protected FluidloggedAPIConfigsEvent(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn) {
        server = serverIn;
        configs = configsIn;
    }

    /**
     *
     * @since 3.0.0
     * @author jbred
     *
     */
    public static abstract class Read extends FluidloggedAPIConfigsEvent
    {
        protected Read(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn) {
            super(serverIn, configsIn);
        }

        /**
         *
         * @since 3.0.0
         * @author jbred
         *
         */
        public static class Pre extends FluidloggedAPIConfigsEvent {
            public Pre(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn) {
                super(serverIn, configsIn);
            }
        }

        /**
         *
         * @since 3.0.0
         * @author jbred
         *
         */
        public static class Post extends FluidloggedAPIConfigsEvent {
            public Post(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn) {
                super(serverIn, configsIn);
            }
        }
    }

    /**
     *
     * @since 3.0.0
     * @author jbred
     *
     */
    public static class Apply extends FluidloggedAPIConfigsEvent
    {
        public final boolean isReload;
        public Apply(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn, final boolean isReloadIn) {
            super(serverIn, configsIn);
            isReload = isReloadIn;
        }
    }
}
