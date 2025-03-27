/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.datafix.modded;

import git.jbredwards.fluidlogged_api.api.datafix.FluidMappingData;
import git.jbredwards.fluidlogged_api.api.datafix.IFluidloggedDataMapper;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.tropicraft.core.registry.BlockRegistry;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public enum TropicraftDataFixer implements IFluidloggedDataMapper
{
    INSTANCE;

    public static void register() {
        MAPPERS.put(BlockRegistry.bambooFence, INSTANCE);
        MAPPERS.put(BlockRegistry.chunkFence, INSTANCE);
        MAPPERS.put(BlockRegistry.mahoganyFence, INSTANCE);
        MAPPERS.put(BlockRegistry.palmFence, INSTANCE);
        MAPPERS.put(BlockRegistry.thatchFence, INSTANCE);
    }

    @Nonnull
    @Override
    public FluidMappingData remapFluidData(final int blockID, final int blockMetadata) {
        return new FluidMappingData(blockMetadata < 2 ? FluidState.of(BlockRegistry.tropicsWater) : FluidState.EMPTY).withMetadata(0);
    }
}
