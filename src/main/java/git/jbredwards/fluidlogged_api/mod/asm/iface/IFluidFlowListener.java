/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.asm.iface;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.IFluidUpdateHelper;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fluids.IFluidBlock;

import javax.annotation.Nonnull;

/**
 * Allows Immersive Engineering's concrete fluid to behave properly while fluidlogged.
 * @author jbred
 *
 */
public interface IFluidFlowListener extends IFluidBlock
{
    boolean preFluidUpdate(@Nonnull final IFluidUpdateHelper helper, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState);

    boolean delayCalculation(@Nonnull final IFluidUpdateHelper helper, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState);

    void postFluidUpdate(@Nonnull final IFluidUpdateHelper helper, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState, final boolean hasFlown);
}
