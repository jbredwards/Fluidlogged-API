/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.network;

import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;

import javax.annotation.Nonnull;

/**
 * Initialized during forge preInit
 * @author jbred
 *
 */
@SuppressWarnings("NotNullFieldNotInitialized")
public class FluidloggedAPINetworkHandler
{
    @Nonnull
    public static SimpleNetworkWrapper INSTANCE;
}
