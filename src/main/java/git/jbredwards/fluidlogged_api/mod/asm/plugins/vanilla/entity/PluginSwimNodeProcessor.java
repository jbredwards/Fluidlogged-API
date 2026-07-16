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
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.EntityLiving;
import net.minecraft.init.Blocks;
import net.minecraft.pathfinding.NodeProcessor;
import net.minecraft.pathfinding.PathNodeType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * account for FluidStates
 * @author jbred
 *
 */
public final class PluginSwimNodeProcessor implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_186318_b" : "getStart") || method.name.equals(obfuscated ? "func_186325_a" : "getPathPointToCoords")) return 1;
        else return method.name.equals(obfuscated ? "func_186327_c" : "isFree") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final ClassNode classNode, @Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        if(index == 1 && checkMethod(insn, obfuscated ? "func_176159_a" : "openPoint")) {
            ((MethodInsnNode)insn).setOpcode(INVOKESPECIAL);
            ((MethodInsnNode)insn).owner = classNode.superName;
            return true;
        }
        else if(index == 2) {
            /*
             * isFree: (changes are around line 71)
             * Old code:
             * IBlockState iblockstate = this.blockaccess.getBlockState(blockpos$mutableblockpos.setPos(i, j, k));
             *
             * New code:
             * // Account for FluidStates.
             * IBlockState iblockstate = Hooks.isFree(this.blockaccess, blockpos$mutableblockpos.setPos(i, j, k), this.entity);
             */
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/pathfinding/NodeProcessor", obfuscated ? "field_186326_b" : "entity", "Lnet/minecraft/entity/EntityLiving;"));
                instructions.insertBefore(insn, genMethodNode("isFree", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/EntityLiving;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        // Helper.
        public static boolean isPassableWater(@Nonnull final IBlockState state, @Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nullable final EntityLiving entity) {
            @Nullable PathNodeType type = state.getBlock().getAiPathNodeType(state, access, pos, entity);
            if(type == PathNodeType.WATER) return true;
            // Check FluidState here if the block allows entities to pass through.
            else if(type == PathNodeType.OPEN || type == null && state.getBlock().isPassable(access, pos)) {
                @Nonnull final FluidState fluidState = FluidloggedUtils.getFluidState(access, pos, state);
                type = fluidState.getBlock().getAiPathNodeType(fluidState.getState(), access, pos, entity);
                return type == PathNodeType.WATER || fluidState.getMaterial() == Material.WATER && (type == PathNodeType.OPEN || type == null && fluidState.getBlock().isPassable(access, pos));
            }

            return false;
        }

        @Nonnull
        public static IBlockState isFree(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final EntityLiving entity) {
            if(isPassableWater(access.getBlockState(pos), access, pos, entity))
                return Blocks.WATER.getDefaultState(); // "WATER".
            else
                return Blocks.AIR.getDefaultState(); // "BLOCKED".
        }
    }
}
