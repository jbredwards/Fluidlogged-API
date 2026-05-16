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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.PluginFluidOrReal;

/**
 * fix ForgeHooks#isInsideOfMaterial by allowing it to access stored fluid blocks
 * @author jbred
 *
 */
public final class PluginForgeHooks extends PluginFluidOrReal
{
    public PluginForgeHooks() {
        /*
         * isInsideOfMaterial: (changes are around line 1034)
         * Old code:
         * IBlockState state = entity.world.getBlockState(pos);
         *
         * New code:
         * //allow FluidStates to be used
         * IBlockState state = FluidloggedUtils.getFluidOrReal(entity.world, pos);
         */
        super("isInsideOfMaterial");
    }
}
