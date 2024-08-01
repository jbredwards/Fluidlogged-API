/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
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
