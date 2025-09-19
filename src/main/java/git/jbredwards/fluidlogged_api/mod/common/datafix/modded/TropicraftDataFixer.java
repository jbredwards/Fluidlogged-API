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
