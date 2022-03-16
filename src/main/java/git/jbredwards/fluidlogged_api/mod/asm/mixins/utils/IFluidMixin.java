package git.jbredwards.fluidlogged_api.mod.asm.mixins.utils;

import git.jbredwards.fluidlogged_api.api.util.FluidState;

import javax.annotation.Nonnull;

/**
 * used by {@link FluidState} to reduce this mod's memory usage
 * @author jbred
 *
 */
public interface IFluidMixin
{
    boolean isEmpty();

    @Nonnull
    FluidState getDefaultFluidState();

    @Nonnull
    FluidState setDefaultFluidState(@Nonnull FluidState fluidState);
}
