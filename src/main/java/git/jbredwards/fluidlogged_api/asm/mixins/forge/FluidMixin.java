package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraftforge.fluids.Fluid;
import org.apache.logging.log4j.Logger;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

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
    private FluidState defaultState = FluidState.EMPTY;

    @Redirect(method = "setBlock", at = @At(value = "INVOKE", target = "Lorg/apache/logging/log4j/Logger;warn(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V", remap = false), remap = false)
    private void warn(@Nonnull Logger instance, @Nonnull String s, @Nonnull Object o1, @Nonnull Object o2, @Nonnull Object o3) {}

    @Override
    public boolean isEmpty() { return defaultState.isEmpty(); }

    @Nonnull
    @Override
    public FluidState getDefaultFluidState() { return defaultState; }

    @Nonnull
    public FluidState setDefaultFluidState(@Nonnull FluidState fluidState) { return defaultState = fluidState; }
}
