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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.PluginFluidOrReal;

/**
 * water FluidStates are now seen as water blocks
 * @author jbred
 *
 */
public final class PluginRandomPositionGenerator extends PluginFluidOrReal
{
    public PluginRandomPositionGenerator() {
        /*
         * isWaterDestination: (changes are around line 179)
         * Old code:
         * return p_191380_1_.world.getBlockState(p_191380_0_).getMaterial() == Material.WATER;
         *
         * New code:
         * //account for FluidStates
         * return FluidloggedUtils.getFluidOrReal(p_191380_1_.world, p_191380_0_).getMaterial() == Material.WATER;
         */
        super("func_191380_b", "isWaterDestination");
    }
}
