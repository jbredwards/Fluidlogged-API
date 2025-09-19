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
