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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.buildcraft;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.PluginFluidOrReal;

/**
 * make buildcraft's pump account for FluidStates when checking for an infinite water source
 * @author jbred
 *
 */
public final class PluginTilePump extends PluginFluidOrReal
{
    public PluginTilePump() {
        /*
         * buildQueue0:
         * Old code:
         * IBlockState below = world.getBlockState(posToCheck.down());
         *
         * New code:
         * // account for FluidStates
         * IBlockState below = FluidloggedUtils.getFluidOrReal(world, posToCheck.down());
         */
        super("buildQueue0");
    }
}
