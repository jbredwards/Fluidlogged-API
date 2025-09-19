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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.mekanism;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * make mekanism's fluid getter methods FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginMekanismUtils implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("getFluid") || method.name.equals("isDeadFluid"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * IBlockState state = pos.getBlockState(world);
         *
         * New code:
         * // make mekanism's fluid getter methods FluidState-sensitive
         * IBlockState state = FluidloggedUtils.getFluidOrReal(world, pos.getPos());
         */
        if(checkMethod(insn, "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "mekanism/api/Coord4D", "getPos", "()Lnet/minecraft/util/math/BlockPos;", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            removeFrom(instructions, insn, -2);
            return true;
        }

        return false;
    }
}
