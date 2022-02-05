package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import git.jbredwards.fluidlogged_api.common.storage.FluidState;

import javax.annotation.Nonnull;

/**
 * used by {@link FluidMixin} to reduce this mod's memory usage
 * @author jbred
 *
 */
public interface IDefaultFluidState
{
    boolean isEmpty();

    @Nonnull
    FluidState getDefaultFluidState();

    @Nonnull
    FluidState setDefaultFluidState(@Nonnull FluidState fluidState);
}
