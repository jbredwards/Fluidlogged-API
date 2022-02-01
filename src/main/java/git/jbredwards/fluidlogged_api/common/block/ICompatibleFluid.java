package git.jbredwards.fluidlogged_api.common.block;

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
     * called by {@link git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils#isCompatibleFluid},
     * which is invoked a lot, so try to keep the code for this fairly light.
     */
    boolean isCompatibleFluid(@Nonnull Fluid otherFluid);
}
