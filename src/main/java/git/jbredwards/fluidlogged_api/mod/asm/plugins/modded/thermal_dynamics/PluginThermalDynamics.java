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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_dynamics;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix block raytrace to ignore fluids
 * @author jbred
 *
 */
public final class PluginThermalDynamics implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("openGui"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * openGui:
         * Old code:
         * RayTraceResult movingObjectPosition = RayTracer.retrace(player);
         *
         * New code:
         * //don't stop the raytrace when it comes in contact with a fluid
         * RayTraceResult movingObjectPosition = RayTracer.retrace(player, false);
         */
        if(checkMethod(insn, "retrace")) {
            //change method to one with a flexible `stopAtFluid` flag & set it to false
            ((MethodInsnNode)insn).desc = "(Lnet/minecraft/entity/player/EntityPlayer;Z)Lnet/minecraft/util/math/RayTraceResult;";
            instructions.insertBefore(insn, new InsnNode(ICONST_0));
            return true;
        }

        return false;
    }
}
