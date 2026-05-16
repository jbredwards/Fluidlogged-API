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

package git.jbredwards.fluidlogged_api.mod.asm.plugins;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * Generic plugin that remaps a World.getBlockState call with FluidloggedUtils.getFluidOrReal.
 * @author jbred
 *
 */
public class PluginFluidOrReal implements IASMPlugin
{
    @Nonnull
    private final String[] names;
    protected boolean onlyFirst = true;

    public PluginFluidOrReal(@Nonnull final String... namesIn) { names = namesIn; }

    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(names.length == 1) return method.name.equals(names[0]);
        for(int i = obfuscated ? 0 : 1; i < names.length; i += 2) if(method.name.equals(names[i])) return true;
        return false;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * worldIn.getBlockState(pos);
         *
         * New code:
         * // account for FluidStates
         * FluidloggedUtils.getFluidOrReal(worldIn, pos);
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return onlyFirst;
        }

        return false;
    }
}
