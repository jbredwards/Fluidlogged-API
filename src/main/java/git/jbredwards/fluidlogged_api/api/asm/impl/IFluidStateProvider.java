package git.jbredwards.fluidlogged_api.api.asm.impl;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.state.IBlockState;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Implemented by {@link net.minecraft.block.state.BlockStateBase BlockStateBase} at runtime to store a default FluidState
 * @author jbred
 *
 */
public interface IFluidStateProvider
{
    @Nonnull
    FluidState getDefaultFluidState();
    void setDefaultFluidState(@Nonnull FluidState fluidState);

    @Nonnull
    static FluidState getDefaultFluidState(@Nullable IBlockState state) {
        return state instanceof IFluidStateProvider ? ((IFluidStateProvider)state).getDefaultFluidState() : FluidState.EMPTY;
    }

    static void setDefaultFluidState(@Nullable IBlockState state, @Nonnull FluidState fluidState) {
        if(state instanceof IFluidStateProvider) ((IFluidStateProvider)state).setDefaultFluidState(fluidState);
    }
}
