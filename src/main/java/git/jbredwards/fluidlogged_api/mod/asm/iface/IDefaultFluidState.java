/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.iface;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.state.IBlockState;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Implemented by {@link net.minecraft.block.state.BlockStateBase BlockStateBase} at runtime to store a default FluidState
 * @author jbred
 *
 */
public interface IDefaultFluidState
{
    @Nullable
    FluidState getDefaultFluidState();
    void setDefaultFluidState(@Nullable final FluidState fluidState);
}
