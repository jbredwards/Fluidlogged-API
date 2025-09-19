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

import git.jbredwards.fluidlogged_api.api.util.FluidState;

import javax.annotation.Nullable;

/**
 * Implemented by {@link net.minecraft.block.state.BlockStateContainer BlockStateContainer} at runtime to store a leve-to-FluidState lookup array
 * @author jbred
 *
 */
public interface ILevelFluidStateLookup
{
    @Nullable
    FluidState[][] getFluidStateLookup();
    void setFluidStateLookup(@Nullable final FluidState[][] fluidStates);
}
