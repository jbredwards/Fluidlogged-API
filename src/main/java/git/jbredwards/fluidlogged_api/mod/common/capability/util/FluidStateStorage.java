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
