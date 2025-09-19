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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tfc;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * duplicate fluid logic isn't needed, and causes conflicts with this mod
 * @author jbred
 *
 */
public final class PluginTFCBlockFluid implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.methods.removeIf(method ->
                method.name.equals(obfuscated ? "func_180650_b" : "updateTick") ||
                method.name.equals("getOptimalFlowDirections") ||
                method.name.equals("calculateFlowCost") ||
                method.name.equals("flowIntoBlock") ||
                method.name.equals("canFlowInto") ||
                method.name.equals("canDisplace") ||
                method.name.equals("getExtendedState") ||
                method.name.equals("getFluidHeightForRender"));

        return false;
    }
}
