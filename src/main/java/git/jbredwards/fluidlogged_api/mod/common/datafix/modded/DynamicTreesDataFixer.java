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

import com.ferreusveritas.dynamictrees.blocks.BlockRootyWater;
import git.jbredwards.fluidlogged_api.api.datafix.FluidMappingData;
import git.jbredwards.fluidlogged_api.api.datafix.IFluidloggedDataMapper;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.registry.ForgeRegistries;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public enum DynamicTreesDataFixer implements IFluidloggedDataMapper
{
    INSTANCE;

    public static void register() {
        ForgeRegistries.BLOCKS.getValuesCollection().stream()
                .filter(block -> block instanceof BlockRootyWater)
                .forEach(block -> MAPPERS.put(block, INSTANCE));
    }

    @Nonnull
    @Override
    public FluidMappingData remapFluidData(final int blockID, final int blockMetadata) {
        return new FluidMappingData(FluidState.of(FluidRegistry.WATER));
    }
}
