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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_expansion;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.PluginFluidOrReal;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * Make Thermal Expansion's machines FluidState-sensitive.
 * @author jbred
 *
 */
public final class PluginThermalExpansion extends PluginFluidOrReal
{
    public PluginThermalExpansion() {
        /*
         * Old code:
         * IBlockState state = this.world.getBlockState(this.pos);
         *
         * New code:
         * // Account for FluidStates.
         * IBlockState state = FluidloggedUtils.getFluidOrReal(this.world, this.pos);
         */
        super(false, true, "updateValidity");
    }
}
