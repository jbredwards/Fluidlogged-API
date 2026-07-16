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
import net.minecraft.pathfinding.PathPoint;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.util.EnumHelper;
import org.apache.commons.lang3.EnumUtils;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;
import java.util.Optional;

/**
 * account for FluidStates and add new PathNodeType
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
             * // Add functionality for new PathNodeType and account for FluidStates.
             * IBlockState iblockstate = Hooks.isFree(this.blockaccess, blockpos$mutableblockpos.setPos(i, j, k), this);
             */
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, genMethodNode("isFree", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/pathfinding/NodeProcessor;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
            }
            /*
             * isFree: (changes are around line 75)
             * Old code:
             * return PathNodeType.BLOCKED;
             *
             * New code:
             * // Add functionality for new PathNodeType.
             * return Hooks.isFree(iblockstate, PathNodeType.BLOCKED);
             */
            else if(checkField(insn, "BLOCKED")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 8));
                instructions.insert(insn, genMethodNode("isFree", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/pathfinding/PathNodeType;)Lnet/minecraft/pathfinding/PathNodeType;"));
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // Add Accessor for super.openPoint().
         * @ASMGenerated
         * public PathPoint fluidlogged_api$super$openPoint(int x, int y, int z)
         * {
         *     return super.openPoint(x, y, z);
         * }
         */
        classNode.interfaces.add(getAccessorClass());
        addMethod(classNode, "fluidlogged_api$super$openPoint", "(III)Lnet/minecraft/pathfinding/PathPoint;", null, null, generator -> {
            generator.loadThis();
            generator.loadArg(0);
            generator.loadArg(1);
            generator.loadArg(2);
            generator.visitMethodInsn(INVOKESPECIAL, classNode.superName, obfuscated ? "func_176159_a" : "openPoint", "(III)Lnet/minecraft/pathfinding/PathPoint;", false);
        });
        /*
         * New code:
         * // Only open path point if entity might actually travel there, and account for FluidStates.
         * @ASMGenerated
         * public PathPoint openPoint(int x, int y, int z)
         * {
         *     return Hooks.openPoint(this, x, y, z);
         * }
         */
        addMethod(classNode, obfuscated ? "func_176159_a" : "openPoint", "(III)Lnet/minecraft/pathfinding/PathPoint;",
            "openPoint", "(Lnet/minecraft/pathfinding/NodeProcessor;III)Lnet/minecraft/pathfinding/PathPoint;", generator -> {
                generator.loadThis();
                generator.loadArg(0);
                generator.loadArg(1);
                generator.loadArg(2);
            }
        );
        /*
         * New code:
         * // Invoke same code as other method.
         * @ASMOverwrite
         * public PathNodeType getPathNodeType(IBlockAccess blockaccessIn, int x, int y, int z, EntityLiving entitylivingIn, int xSize, int ySize, int zSize, boolean canBreakDoorsIn, boolean canEnterDoorsIn)
         * {
         *     return this.getPathNodeType(blockaccessIn, x, y, z);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_186319_a" : "getPathNodeType", "(Lnet/minecraft/world/IBlockAccess;IIILnet/minecraft/entity/EntityLiving;IIIZZ)Lnet/minecraft/pathfinding/PathNodeType;"), null, null, generator -> {
            generator.loadThis();
            generator.loadArg(0);
            generator.loadArg(1);
            generator.loadArg(2);
            generator.loadArg(3);
            generator.visitMethodInsn(INVOKEVIRTUAL, classNode.name, obfuscated ? "func_186330_a" : "getPathNodeType", "(Lnet/minecraft/world/IBlockAccess;III)Lnet/minecraft/pathfinding/PathNodeType;", false);
        });
        /*
         * New code:
         * // Account for FluidStates, and non-passable blocks.
         * @ASMOverwrite
         * public PathNodeType getPathNodeType(IBlockAccess blockaccessIn, int x, int y, int z)
         * {
         *     return Hooks.getPathNodeType(this, blockaccessIn, x, y, z);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_186330_a" : "getPathNodeType", "(Lnet/minecraft/world/IBlockAccess;III)Lnet/minecraft/pathfinding/PathNodeType;"),
            "getPathNodeType", "(Lnet/minecraft/pathfinding/NodeProcessor;Lnet/minecraft/world/IBlockAccess;III)Lnet/minecraft/pathfinding/PathNodeType;", generator -> {
                generator.loadThis();
                generator.loadArg(0);
                generator.loadArg(1);
                generator.loadArg(2);
                generator.loadArg(3);
            }
        );

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        private static final PathNodeType BREACH = Optional.ofNullable(EnumUtils.getEnum(PathNodeType.class, "BREACH")).orElseGet(() ->
                Objects.requireNonNull(EnumHelper.addEnum(PathNodeType.class, "BREACH", new Class[]{float.class}, 4f)));

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
        public static PathNodeType getPathNodeType(@Nonnull final NodeProcessor processor, @Nonnull final IBlockAccess access, final int x, final int y, final int z) {
            @Nonnull final BlockPos pos = new BlockPos(x, y, z);
            @Nonnull final IBlockState state = access.getBlockState(pos);
            return isPassableWater(state, access, pos, processor.entity) ? PathNodeType.WATER : PathNodeType.BLOCKED;
        }

        @Nonnull
        public static IBlockState isFree(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final NodeProcessor processor) {
            if(isPassableWater(access.getBlockState(pos), access, pos, processor.entity))
                return Blocks.WATER.getDefaultState(); // "WATER".

            else if(isPassableWater(access.getBlockState(pos.down()), access, pos.down(), processor.entity))
                return Blocks.LAVA.getDefaultState(); // "BREACH".

            else
                return Blocks.AIR.getDefaultState(); // "BLOCKED".
        }

        @Nonnull
        public static PathNodeType isFree(@Nonnull final IBlockState state, @Nonnull final PathNodeType blocked) {
            return state.getBlock() == Blocks.LAVA ? BREACH : blocked;
        }

        @Nullable
        public static PathPoint openPoint(@Nonnull final NodeProcessor processor, final int x, final int y, final int z) {
            @Nonnull final PathNodeType type = processor.getPathNodeType(processor.blockaccess, x, y, z);
            final float priority = processor.entity.getPathPriority(type);
            if(priority >= 0) {
                @Nonnull final PathPoint point = ((Accessor)processor).fluidlogged_api$super$openPoint(x, y, z);
                point.costMalus = Math.max(point.costMalus, priority);
                point.nodeType = type;

                if(FluidloggedUtils.getFluidState(processor.blockaccess, new BlockPos(x, y, z)).isEmpty()) point.costMalus += 8;
                return point;
            }

            return null;
        }
    }

    public interface Accessor
    {
        @Nonnull
        PathPoint fluidlogged_api$super$openPoint(final int x, final int y, final int z);
    }
}
