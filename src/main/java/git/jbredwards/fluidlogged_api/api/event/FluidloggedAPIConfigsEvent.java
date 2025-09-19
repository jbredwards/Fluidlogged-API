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

package git.jbredwards.fluidlogged_api.api.event;

import com.google.gson.JsonObject;
import net.minecraft.server.MinecraftServer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;

/**
 * All children of this event are fired on the {@link MinecraftForge#EVENT_BUS} serverside.<br>
 * All children of this event are {@link Cancelable cancelable}.
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
     * All children of this event are fired on the {@link MinecraftForge#EVENT_BUS} serverside.<br>
     * All children of this event are {@link Cancelable cancelable}.
     *
     * @since 3.0.0
     * @author jbred
     *
     */
    @Cancelable
    public static abstract class Read extends FluidloggedAPIConfigsEvent
    {
        protected Read(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn) {
            super(serverIn, configsIn);
        }

        /**
         * This event is fired on the {@link net.minecraftforge.common.MinecraftForge#EVENT_BUS} serverside before
         * {@link git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs#readConfigFiles readConfigFiles()}
         * reads and puts all whitelist/blacklist/fluidTag config file data to the configs JsonObject.<br>
         * This event is {@link Cancelable cancelable}.
         *
         * @since 3.0.0
         * @author jbred
         *
         */
        @Cancelable
        public static class Pre extends FluidloggedAPIConfigsEvent {
            public Pre(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn) {
                super(serverIn, configsIn);
            }
        }

        /**
         * This event is fired on the {@link net.minecraftforge.common.MinecraftForge#EVENT_BUS} serverside after
         * {@link git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs#readConfigFiles readConfigFiles()}
         * reads and puts all whitelist/blacklist/fluidTag config file data to the configs JsonObject.<br>
         * This event is {@link Cancelable cancelable}.
         *
         * @since 3.0.0
         * @author jbred
         *
         */
        @Cancelable
        public static class Post extends FluidloggedAPIConfigsEvent {
            public Post(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn) {
                super(serverIn, configsIn);
            }
        }
    }

    /**
     * This event is fired on the {@link net.minecraftforge.common.MinecraftForge#EVENT_BUS} serverside before
     * {@link git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs#init init()} is called
     * and before the new config data is sent to connected clients.<br>
     * This event is {@link Cancelable cancelable}. Cancelling this event will prevent
     * {@link git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs#init init()}
     * from being called and prevent the new config data from being sent to connected clients.
     *
     * @since 3.0.0
     * @author jbred
     *
     */
    @Cancelable
    public static class Apply extends FluidloggedAPIConfigsEvent
    {
        public final boolean isReload;
        public Apply(@Nonnull final MinecraftServer serverIn, @Nonnull final JsonObject configsIn, final boolean isReloadIn) {
            super(serverIn, configsIn);
            isReload = isReloadIn;
        }
    }
}
