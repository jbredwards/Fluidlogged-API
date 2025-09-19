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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.chiselsandbits;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.tileentity.TileEntity;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * return null by default, to use built-in fluid collision logic for non-fluid chisel blocks
 * @author jbred
 *
 */
public final class PluginBlockChiseled implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("sharedIsAABBInsideMaterial") || method.name.equals("sharedIsEntityInsideMaterial"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * sharedIsAABBInsideMaterial & sharedIsEntityInsideMaterial:
         * Old code:
         * return Boolean.valueOf(false)
         *
         * New code:
         * // Fall back on the FluidState collision check, for non-fluid chisel blocks
         * return Hooks.getDefaultReturn(tebc)
         */
        if(insn.getOpcode() == ICONST_0 && checkMethod(insn.getNext(), "valueOf")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, genMethodNode("getDefaultReturn", "(Lnet/minecraft/tileentity/TileEntity;)Ljava/lang/Boolean;"));
            removeFrom(instructions, insn, 1);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static Boolean getDefaultReturn(@Nonnull final TileEntity tile) { return FluidloggedUtils.isFluid(tile.getBlockType()) ? Boolean.FALSE : null; }
    }
}
