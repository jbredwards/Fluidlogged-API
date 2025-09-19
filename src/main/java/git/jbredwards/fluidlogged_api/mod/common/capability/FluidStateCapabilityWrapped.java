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
