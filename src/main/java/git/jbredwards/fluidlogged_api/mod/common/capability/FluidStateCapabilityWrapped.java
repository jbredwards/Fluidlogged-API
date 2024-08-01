/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.capability;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IHardcodedCapability;
import net.minecraft.nbt.NBTBase;

import javax.annotation.Nonnull;

/**
 * Attached to Chunks to save/read existing data using forge's capability system.
 * @author jbred
 *
 */
public class FluidStateCapabilityWrapped implements IFluidStateCapability
{
    @Nonnull
    protected final IHardcodedCapability wrapped;
    public FluidStateCapabilityWrapped(@Nonnull final IHardcodedCapability wrappedIn) { wrapped = wrappedIn; }

    @Nonnull
    @Override
    public IFluidStateContainer getContainer(final int y) { return wrapped.getFluidStateCapability().getContainer(y); }

    @Nonnull
    @Override
    public NBTBase serializeNBT() { return wrapped.getFluidStateCapability().serializeNBT(); }

    @Override
    public void deserializeNBT(@Nonnull final NBTBase nbt) { wrapped.getFluidStateCapability().deserializeNBT(nbt); }
}
