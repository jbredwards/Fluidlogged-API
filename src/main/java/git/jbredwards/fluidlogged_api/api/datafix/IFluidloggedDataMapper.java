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

package git.jbredwards.fluidlogged_api.api.datafix;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import net.minecraft.block.Block;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Converts data for "pseudo-fluidlogged" blocks into real FluidState data, with the option of changing the block metadata.
 * Useful for mods that want to migrate to a Fluidlogged API dependency.
 *
 * @since 3.0.0
 * @author jbred
 *
 */
@FunctionalInterface
public interface IFluidloggedDataMapper
{
    /**
     * Holds all active {@link IFluidloggedDataMapper IFluidloggedDataMappers}, used by
     * {@link git.jbredwards.fluidlogged_api.mod.common.datafix.ToFluidloggedDataFixer ToFluidloggedDataFixer}.
     * <p>All entries should be finalized by the fml post-init phase.</p>
     *
     * @since 3.0.0
     */
    @Nonnull
    Multimap<Block, IFluidloggedDataMapper> MAPPERS = HashMultimap.create();

    /**
     * @param blockID The old block id. Useful for remapped blocks.
     * @param blockMetadata The block metadata.
     * @return A pair of [an optional new metadata value for the block, and the FluidState].
     * To remap a block, use Forge's {@link net.minecraftforge.event.RegistryEvent.MissingMappings MissingMappings} event.
     * If you want to map a block that was remapped: cache its
     * {@link net.minecraftforge.event.RegistryEvent.MissingMappings.Mapping#id old ID} and compare it with the blockID
     * provided, and use the new block as the key for the {@link IFluidloggedDataMapper#MAPPERS} entry.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nullable
    FluidMappingData remapFluidData(final int blockID, final int blockMetadata);
}
