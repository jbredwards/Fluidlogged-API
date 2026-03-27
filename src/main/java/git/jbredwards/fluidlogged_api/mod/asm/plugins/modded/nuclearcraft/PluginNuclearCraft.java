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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.nuclearcraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * make source hot fluid blocks only vaporize their FluidState while fluidlogged instead of the whole block
 * @author jbred
 *
 */
public final class PluginNuclearCraft implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_180650_b" : "updateTick"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * updateTick:
         * Old code:
         * worldIn.setBlockToAir(pos);
         *
         * New code:
         * // only vaporize the fluid
         * FluidloggedUtils.setFluidToAir(worldIn, pos, null, 3);
         */
        if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
            instructions.insertBefore(insn, new InsnNode(ACONST_NULL));
            instructions.insertBefore(insn, new InsnNode(ICONST_3));
            instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "setFluidToAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
