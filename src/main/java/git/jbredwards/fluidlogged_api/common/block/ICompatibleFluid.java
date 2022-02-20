package git.jbredwards.fluidlogged_api.common.block;

import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraftforge.fluids.Fluid;

import javax.annotation.Nonnull;

/**
 * useful for fluids blocks whose logic should act as one (fixes issue#44)
 * @author jbred
 *
 */
public interface ICompatibleFluid
{
    /**
     * called by {@link FluidloggedUtils#isCompatibleFluid},
     * which is invoked a lot, so try to keep the code for this fairly light.
     */
    default boolean isCompatibleFluid(@Nonnull Fluid otherFluid) {
        return FluidloggedUtils.isCompatibleFluid(getParentFluid(), otherFluid);
    }

    /**
     * allows cross-mod compatibility with no dependencies
     * (example, water-like fluids would return FluidRegistry.WATER)
     */
    @Nonnull Fluid getParentFluid();
}
