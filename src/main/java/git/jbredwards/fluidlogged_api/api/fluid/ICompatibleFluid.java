package git.jbredwards.fluidlogged_api.api.fluid;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.Fluid;

import javax.annotation.Nonnull;

/**
 * useful for fluids whose logic should act as one (fixes issue#44)
 * @author jbred
 *
 */
public interface ICompatibleFluid
{
    /**
     * allows cross-mod compatibility with no dependencies
     * (example, water-like fluids would return FluidRegistry.WATER)
     */
    @Nonnull
    Fluid getParentFluid(@Nonnull IBlockAccess world);

    /**
     * called by {@link FluidloggedUtils#isCompatibleFluid},
     * which is invoked a lot, so try to keep the code for this fairly light.
     */
    default boolean isCompatibleFluid(@Nonnull IBlockAccess world, @Nonnull Fluid otherFluid) {
        //use recursion by default in case the parent & otherFluid are compatible
        //ie every fluid doesn't have to have hardcoded compat
        return FluidloggedUtils.isCompatibleFluid(world, getParentFluid(world), otherFluid);
    }
}
