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

package git.jbredwards.fluidlogged_api.mod.common.capability.util;

import git.jbredwards.fluidlogged_api.api.util.FluidState;

import javax.annotation.Nonnull;

/**
 * Holds the data for FluidStates within a 16x1x16 area
 * @author jbred
 *
 */
public class FluidStateLayer
{
    @Nonnull
    public FluidState[] data = new FluidState[256];
    public byte tracker = Byte.MIN_VALUE; //start with min byte value, to prevent possible overflow problems
}
