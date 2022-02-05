package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import git.jbredwards.fluidlogged_api.common.storage.FluidState;
import net.minecraftforge.fluids.Fluid;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * reduce memory usage when storing FluidStates in Chunk capabilities
 * by storing one instance, rather than (possibly thousands of) duplicates
 * @author jbred
 *
 */
@Mixin(Fluid.class)
public abstract class FluidMixin implements IDefaultFluidState
{
    @Nonnull
    FluidState defaultState = FluidState.EMPTY;

    @Override
    public boolean isEmpty() { return defaultState.isEmpty(); }

    @Nonnull
    @Override
    public FluidState getDefaultFluidState() { return defaultState; }

    @Nonnull
    @Override
    public FluidState setDefaultFluidState(@Nonnull FluidState fluidState) { return defaultState = fluidState; }
}
