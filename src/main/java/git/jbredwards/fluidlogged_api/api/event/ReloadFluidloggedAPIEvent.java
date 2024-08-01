/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.event;

import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

/**
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public abstract class ReloadFluidloggedAPIEvent extends Event
{
    public final boolean isReload;
    protected ReloadFluidloggedAPIEvent(final boolean isReloadIn) { isReload = isReloadIn; }

    /**
     *
     * @since 3.0.0
     * @author jbred
     *
     */
    @Cancelable
    public static class Pre extends ReloadFluidloggedAPIEvent {
        public Pre(final boolean isReloadIn) { super(isReloadIn); }
    }

    /**
     *
     * @since 3.0.0
     * @author jbred
     *
     */
    @Cancelable
    public static class Post extends ReloadFluidloggedAPIEvent {
        public Post(final boolean isReloadIn) { super(isReloadIn); }
    }
}
