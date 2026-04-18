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
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.ICubeData;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * concrete forms from concrete powder while its next to flowing water FluidStates
 * @author jbred
 *
 */
public final class PluginBlockConcretePowder implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_176502_a_" : "onEndFalling")) return 1;
        else return method.name.equals(obfuscated ? "func_192425_e" : "tryTouchWater") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * onEndFalling: (changes are around line 32)
         * Old code:
         * if (hitState.getMaterial().isLiquid())
         * {
         *     ...
         * }
         *
         * New code:
         * //check for FluidState
         * if (FluidloggedUtils.getFluidOrReal(worldIn, pos, hitState).getMaterial().isLiquid())
         * {
         *     ...
         * }
         */
        if(index == 1 && insn.getOpcode() == ALOAD) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            return true;
        }
        /*
         * tryTouchWater: (changes are around line 48)
         * Old code:
         * if (worldIn.getBlockState(blockpos).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         *
         * New code:
         * //
         * if (Hooks.tryTouchWater(worldIn, blockpos, enumfacing)))
         * {
         *     ...
         * }
         */
        else if(index == 2 && checkField(insn, obfuscated ? "field_151586_h" : "WATER")) {
            ((JumpInsnNode)insn.getNext()).setOpcode(IFEQ);
            instructions.insert(insn, genMethodNode("tryTouchWater", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 8));
            removeFrom(instructions, insn, -2);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean tryTouchWater(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumFacing facing) {
            @Nonnull final ICubeData cube = ICubeData.get(world, pos);
            @Nonnull final IBlockState state = cube.getBlockState(pos);

            return FluidloggedUtils.getFluidState(cube, pos, state).getMaterial() == Material.WATER && (!FluidloggedAPIConfig.fixBadFluidMixing || FluidloggedUtils.canFluidFlow(world, pos, state, facing.getOpposite()));
        }
    }
}
