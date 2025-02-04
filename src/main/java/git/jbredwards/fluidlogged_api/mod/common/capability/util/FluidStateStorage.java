/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.capability.util;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import net.minecraft.nbt.NBTBase;
import net.minecraft.util.EnumFacing;
import net.minecraftforge.common.capabilities.Capability;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public enum FluidStateStorage implements Capability.IStorage<IFluidStateCapability>
{
    INSTANCE;

    @Nonnull
    @Override
    public NBTBase writeNBT(@Nullable final Capability<IFluidStateCapability> capability, @Nonnull final IFluidStateCapability instance, @Nullable final EnumFacing side) {
        return instance.serializeNBT();
    }

    @Override
    public void readNBT(@Nullable final Capability<IFluidStateCapability> capability, @Nonnull final IFluidStateCapability instance, @Nullable final EnumFacing side, @Nonnull final NBTBase nbtIn) {
        instance.deserializeNBT(nbtIn);
    }
}
