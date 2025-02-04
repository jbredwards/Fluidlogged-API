/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.fluid;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.world.World;

import javax.annotation.Nonnull;

/**
 * Have your fluid block implement this if it should have a flow cost that isn't 1.
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public interface IFlowCostFluid
{
    /**
     * The flow cost of a fluid is the amount of {@link net.minecraft.block.BlockLiquid#LEVEL levels} that fluid blocks lose the further it is from its source block (or from a waterfall).
     * For example, water has a flow cost of 1, and lava has a flow cost of 2 (or in the nether, lava has a flow cost of 1).
     * <p><b>
     * The flow cost for any fluid block must be a factor of its {@link net.minecraftforge.fluids.BlockFluidBase#quantaPerBlock quantaPerBlock}.
     * </b></p>
     *
     * @param fluidState FluidState.
     * @param world World.
     * @return The flow cost for this fluid block.
     * @throws NullPointerException If fluidState or world are null.
     *
     * @since 3.0.0
     * @author jbred
     */
    int getFlowCost(@Nonnull final FluidState fluidState, @Nonnull final World world);
}
