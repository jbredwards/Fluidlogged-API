/*
 * Copyright (C) <2026 to Present> <jbredwards>
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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.rsgauges;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.TypeInsnNode;

import javax.annotation.Nonnull;

/**
 * Allow non-World IBlockAccess instances to read redstone signal strength
 * @author jbred
 *
 */
public final class PluginBlockSwitch implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("getPower"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * if ((this.config & 0x400000000000000L) != 0L || !(world instanceof World))
         * {
         *     return 0;
         * }
         *
         * New code:
         * // Allow non-World IBlockAccess instances to read redstone signal strength.
         * if ((this.config & 0x400000000000000L) != 0L || !(world instanceof IBlockAccess))
         * {
         *     return 0;
         * }
         */
        if(insn.getOpcode() == INSTANCEOF && ((TypeInsnNode)insn).desc.equals("net/minecraft/world/World")) ((TypeInsnNode)insn).desc = "net/minecraft/world/IBlockAccess";
        else if(insn.getOpcode() == CHECKCAST && ((TypeInsnNode)insn).desc.equals("net/minecraft/world/World")) instructions.remove(insn);
        return false;
    }
}
