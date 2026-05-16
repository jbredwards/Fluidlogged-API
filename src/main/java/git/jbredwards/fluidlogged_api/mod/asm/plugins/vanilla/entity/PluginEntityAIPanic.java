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
public final class PluginEntityAIPanic extends PluginFluidOrReal
{
    public PluginEntityAIPanic() {
        /*
         * getRandPos: (changes are around line 106):
         * Old code:
         * IBlockState iblockstate = worldIn.getBlockState(blockpos$mutableblockpos);
         *
         * New code:
         * //account for FluidStates
         * IBlockState iblockstate = FluidloggedUtils.getFluidOrReal(worldIn, blockpos$mutableblockpos);
         */
        super("func_188497_a", "getRandPos");
    }
}
