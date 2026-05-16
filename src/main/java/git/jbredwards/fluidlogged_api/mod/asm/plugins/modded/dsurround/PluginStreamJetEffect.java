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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dsurround;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * Account for FluidStates and side solidity.
 * @author jbred
 *
 */
public final class PluginStreamJetEffect implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(method.name.equals("isValidSpawnBlock")) return method.desc.equals("(Lorg/orecruncher/lib/chunk/IBlockAccessEx;Lnet/minecraft/util/math/BlockPos;)Z") ? 2 : 3;
        else return method.name.equals("lavaCount") || method.name.equals("countBlocks") ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * lavaCount:
         * Old code:
         * IBlockState theBlock = provider.getBlockState(pos.getX() + i, pos.getY() + j, pos.getZ() + k);
         *
         * New code:
         * // account for FluidStates
         * IBlockState theBlock = Hooks.getFluidOrReal(provider, pos.getX() + i, pos.getY() + j, pos.getZ() + k);
         */
        if(index == 1 && checkMethod(insn, "getBlockState")) {
            instructions.insert(insn, genMethodNode("getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;III)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * isValidSpawnBlock:
         * Old code:
         * provider.getBlockState(pos);
         *
         * New code:
         * // account for FluidStates
         * FluidloggedUtils.getFluidOrReal(provider, pos);
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * isValidSpawnBlock:
         * Old code:
         * if (state.getMaterial().isLiquid() && provider.isAirBlock(pos.up()))
         *
         * New code:
         * // account for side solidity
         * if (Hooks.canFlow(state.getMaterial().isLiquid(), provider, pos) && provider.isAirBlock(pos.up()))
         */
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_76224_d" : "isLiquid")) {
            instructions.insert(insn, genMethodNode("canFlow", "(ZLnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canFlow(final boolean isLiquid, @Nonnull final IBlockAccess access, @Nonnull final BlockPos pos) {
            return isLiquid && FluidloggedUtils.canFluidConnect(access, pos, access.getBlockState(pos), EnumFacing.UP);
        }

        @Nonnull
        public static IBlockState getFluidOrReal(@Nonnull final IBlockAccess access, final int x, final int y, final int z) {
            return FluidloggedUtils.getFluidOrReal(access, new BlockPos(x, y, z));
        }
    }
}
