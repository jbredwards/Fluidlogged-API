/*
 * Copyright (C) <2026 to Present> <jbredwards>
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

package git.jbredwards.fluidlogged_api.api.world;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.util.math.Vec3i;

import javax.annotation.Nonnull;

/**
 * Gives IBlockAccess instances the option to provide ICubeData in a way that this mod can access them.
 *
 * @since 3.3.0
 * @author jbred
 *
 */
public interface ICubeDataProvider extends IFluidStateProvider
{
    /**
     * @param chunkX X position of the cube (chunk coords, not block coords).
     * @param chunkY Y position of the cube (chunk coords, not block coords).
     * @param chunkZ Z position of the cube (chunk coords, not block coords).
     * @return A 16x16x16 view of this IBlockAccess at the given chunk coords.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    ICubeData getCubeData(final int chunkX, final int chunkY, final int chunkZ);

    /**
     * @param pos Position.
     * @return A 16x16x16 view of this IBlockAccess encompassing the given coords.
     * @throws NullPointerException If pos is null.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    default ICubeData getCubeData(@Nonnull final Vec3i pos) {
        return getCubeData(pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4);
    }

    /**
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @return The fluid state at the position, or {@link FluidState#EMPTY} if none is present.
     *
     * @since 3.3.0
     * @author jbred
     */
    @Nonnull
    @Override
    default FluidState getFluidState(final int x, final int y, final int z) {
        return getCubeData(x >> 4, y >> 4, z >> 4).getFluidState(x, y, z);
    }
}
