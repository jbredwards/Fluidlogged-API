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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.appliedenergistics2;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.PluginFluidOrReal;

/**
 * make ae2's fluix crystal creation and crystal seed growth FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginAE2 extends PluginFluidOrReal
{
    public PluginAE2() {
        /*
         * onUpdate:
         * Old code:
         * IBlockState state = this.world.getBlockState(new BlockPos(j, i, k));
         *
         * New code:
         * // account for FluidStates
         * IBlockState state = FluidloggedUtils.getFluidOrReal(this.world, new BlockPos(j, i, k));
         */
        super("func_70071_h_", "onUpdate");
    }
}
