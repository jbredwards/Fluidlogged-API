/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.iface;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.IFluidBlock;

import javax.annotation.Nonnull;

/**
 * Allows thermal foundation's fluid flow limitation behavior to be added to fluidlogged api.
 * @author jbred
 *
 */
public interface IConditionalFluid extends IFluidBlock
{
    boolean cannotFlowAt(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState);
}
