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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * account for FluidStates
 * @author jbred
 *
 */
public final class PluginWalkNodeProcessor implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_189553_b" : "getPathNodeTypeRaw"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * getPathNodeTypeRaw: (changes are around lines 500 & 504)
         * Old code:
         * if (material == Material.WATER)
         * {
         *     return ...;
         * }
         * else if (material == Material.LAVA)
         * {
         *     return ...;
         * }
         *
         * New code:
         * // Account for FluidStates.
         * if (material == Hooks.checkFluidMaterial(material, p_189553_1_, blockpos, iblockstate, Material.WATER))
         * {
         *     return ...;
         * }
         * else if (material == Hooks.checkFluidMaterial(material, p_189553_1_, blockpos, iblockstate, Material.LAVA))
         * {
         *     return ...;
         * }
         */
        final boolean isLava = checkField(insn, obfuscated ? "field_151587_i" : "LAVA", "Lnet/minecraft/block/material/Material;");
        if(isLava || checkField(insn, obfuscated ? "field_151586_h" : "WATER", "Lnet/minecraft/block/material/Material;")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 8));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 5));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 6));
            instructions.insert(insn, genMethodNode("checkFluidMaterial", "(Lnet/minecraft/block/material/Material;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/material/Material;)Lnet/minecraft/block/material/Material;"));
            return isLava;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static Material checkFluidMaterial(@Nonnull final Material material, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final Material target) {
            return material == target || FluidloggedUtils.getFluidOrReal(world, pos, state).getMaterial() == target ? material : target;
        }
    }
}
