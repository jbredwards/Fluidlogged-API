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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * trapdoors now notify neighbors when opening/closing
 * @author jbred
 *
 */
public final class PluginBlockTrapDoor implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_180639_a" : "onBlockActivated") || method.name.equals(obfuscated ? "func_189540_a" : "neighborChanged"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * onBlockActivated & neighborChanged: (changes are around lines 122 & 159)
         * Old code:
         * worldIn.setBlockState(pos, state, 2);
         * ...
         * worldIn.setBlockState(pos, state.withProperty(OPEN, Boolean.valueOf(flag)), 2);
         *
         * New code:
         * // notify neighbors of state change
         * worldIn.setBlockState(pos, state, 3);
         * ...
         * worldIn.setBlockState(pos, state.withProperty(OPEN, Boolean.valueOf(flag)), 3);
         */
        if(insn.getOpcode() == ICONST_2 && checkMethod(insn.getNext(), obfuscated ? "func_180501_a" : "setBlockState")) {
            instructions.insertBefore(insn, new InsnNode(ICONST_3));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
