/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.iface;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;

import javax.annotation.Nonnull;

/**
 * Implemented by {@link net.minecraft.world.chunk.Chunk Chunk} to greatly improve performance
 * @author jbred
 *
 */
public interface IHardcodedCapability
{
    @Nonnull
    IFluidStateCapability getFluidStateCapability();
    void setFluidStateCapability(@Nonnull final IFluidStateCapability cap);
}
