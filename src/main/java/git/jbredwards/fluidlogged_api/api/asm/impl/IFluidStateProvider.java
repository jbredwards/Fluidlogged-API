package git.jbredwards.fluidlogged_api.api.asm.impl;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraftforge.fluids.Fluid;

import javax.annotation.Nonnull;

/**
 * Implemented by {@link Fluid} at runtime to store a default FluidState
 * @author jbred
 *
 */
public interface IFluidStateProvider
{
    @Nonnull
    FluidState getDefaultFluidState();
    void setDefaultFluidState(@Nonnull FluidState fluidState);

    @Nonnull
    static FluidState getDefaultFluidState(@Nonnull Fluid fluid) {
        return ((IFluidStateProvider)fluid).getDefaultFluidState();
    }

    static void setDefaultFluidState(@Nonnull Fluid fluid, @Nonnull FluidState fluidState) {
        ((IFluidStateProvider)fluid).setDefaultFluidState(fluidState);
    }
}
