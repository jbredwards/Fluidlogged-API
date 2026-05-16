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
 * fishhook entities generate the fishing particles at water FluidStates
 * @author jbred
 *
 */
public final class PluginEntityFishHook extends PluginFluidOrReal
{
    public PluginEntityFishHook() {
        /*
         * onUpdate & cachingFish: (changes are around lines 174, 428, and 478)
         * Old code:
         * IBlockState iblockstate = this.world.getBlockState(blockpos);
         *
         * New code:
         * //account for FluidStates
         * IBlockState iblockstate = FluidloggedUtils.getFluidOrReal(this.world, blockpos);
         */
        super("func_70071_h_", "onUpdate", "func_190621_a", "catchingFish");
        onlyFirst = false;
    }
}
