/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.datafix.tropicraft;

import git.jbredwards.fluidlogged_api.api.datafix.IFluidloggedDataMapper;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.tropicraft.core.registry.BlockRegistry;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import java.util.OptionalInt;

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
    public Pair<OptionalInt, FluidState> remapFluidData(final int blockID, final int blockMetadata) {
        return Pair.of(OptionalInt.of(0), blockMetadata < 2 ? FluidState.of(BlockRegistry.tropicsWater) : FluidState.EMPTY);
    }
}
