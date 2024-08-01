/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
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
