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

package git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks;

import git.jbredwards.fluidlogged_api.mod.common.capability.FluidStateCapabilityVanilla;
import io.github.opencubicchunks.cubicchunks.api.util.XYZAddressable;

import javax.annotation.Nonnull;

/**
 * Cubic Chunks mod compat, holds FluidStates within a 16x16x16 area
 * @author jbred
 *
 */
public class FluidStateCapabilityICube extends FluidStateCapabilityVanilla
{
    protected final int offsetY;
    public FluidStateCapabilityICube(@Nonnull final XYZAddressable cube) {
        super(cube.getX(), cube.getZ());
        offsetY = cube.getY() << 4;
    }

    @Override
    public int serializeY(final int y) { return (y & 15) << 8; }

    @Override
    public int deserializeY(final char serializedPos) { return offsetY | ((serializedPos >> 8) & 15); }
}
