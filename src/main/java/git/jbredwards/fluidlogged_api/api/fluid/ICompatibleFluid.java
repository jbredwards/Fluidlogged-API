/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.fluid;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraftforge.fluids.Fluid;

import javax.annotation.Nonnull;

/**
 * Have your fluid (not fluid block) implement this if it should treat other fluids as "equal". See
 * <a href="https://github.com/jbredwards/Fluidlogged-API/issues/44">issue#44</a> for more info.
 * <p>Use {@link net.minecraftforge.fml.common.Optional Forge's Optional @interfaces} to prevent a required Fluidlogged API dependency.</p>
 *
 * @since 1.7.0
 * @author jbred
 *
 */
public interface ICompatibleFluid
{
    /**
     * Allows cross-mod compatibility with no dependencies
     * (example, water-like fluids would return FluidRegistry.WATER)
     *
     * @since 1.8.0
     * @author jbred
     */
    @Nonnull
    Fluid getParentFluid();

    /**
     * Called by {@link ICompatibleFluid#getFluidCompatibility},
     * which is invoked a lot, so try to keep the code for this fairly light.
     *
     * @param otherFluid Fluid to compare with this one.
     * @return True if this fluid is compatible with otherFluid.
     * @throws NullPointerException If otherFluid is null.
     *
     * @since 1.8.0
     * @author jbred
     */
    default boolean isCompatibleFluid(@Nonnull final Fluid otherFluid) {
        // use recursion by default in case the parent & otherFluid are compatible
        // ie every fluid doesn't have to have hardcoded compat
        return FluidloggedUtils.isCompatibleFluid(getParentFluid(), otherFluid);
    }

    /**
     * Called by {@link FluidloggedUtils#isCompatibleFluid},
     * which is invoked a lot, so try to keep the code for this fairly light.
     *
     * @param otherFluid Fluid to compare with this one.
     * @return A number > 0 to specify fluid compatibility, or a number <= 0 to specify incompatibility.
     * If otherFluid is also an instance of ICompatibleFluid,
     * <pre>{@code
     * final int compat1 = ((ICompatibleFluid)fluid1).getFluidCompatibility(fluid2);
     * final int compat2 = ((ICompatibleFluid)fluid2).getFluidCompatibility(fluid1);
     * return Math.max(compat1, compat2) - Math.min(compat1, compat2);
     * }</pre>
     * is used instead.
     *
     * @throws NullPointerException If otherFluid is null.
     * @since 3.0.0
     * @author jbred
     */
    default int getFluidCompatibility(@Nonnull final Fluid otherFluid) { return isCompatibleFluid(otherFluid) ? 1 : 0; }
}
