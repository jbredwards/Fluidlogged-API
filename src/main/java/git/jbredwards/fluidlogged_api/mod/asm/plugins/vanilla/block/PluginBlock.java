/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.BlockLilyPad;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.IPlantable;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes some lighting, canSustainPlant, and explosion related issues
 * @author jbred
 *
 */
public final class PluginBlock implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_176200_f" : "isReplaceable"))
            return 1;
        else if(checkMethod(method, "removedByPlayer", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/player/EntityPlayer;Z)Z"))
            return 2;
        else if(checkMethod(method, "canSustainPlant", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraftforge/common/IPlantable;)Z"))
            return 4;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * isReplaceable: (changes are around line 445)
         * Old code:
         * return worldIn.getBlockState(pos).getMaterial().isReplaceable();
         *
         * New code:
         * //use this material to improve performance
         * return this.material.isReplaceable();
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_185904_a" : "getMaterial")) {
            //add blockMaterial#isReplaceable
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            removeFrom(instructions, insn, -3);
            return true;
        }
        /*
         * removedByPlayer: (changes are around line 1533)
         * Old code:
         * return world.setBlockState(pos, net.minecraft.init.Blocks.AIR.getDefaultState(), world.isRemote ? 11 : 3);
         *
         * New code:
         * //when a block is removed by a player, set the FluidState here and notify (if it's empty, air is set instead)
         * return PluginWorld.Hooks.setBlockToAir(world, pos, net.minecraft.init.Blocks.AIR.getDefaultState(), world.isRemote ? 11 : 3);
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/world/PluginWorld$Hooks", "setBlockToAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
            return true;
        }
        else if(index == 4) {
            /*
             * canSustainPlant: (changes are around line 2060)
             * Old code:
             * if (plantable instanceof BlockBush && ((BlockBush)plantable).canSustainBush(state))
             * {
             *     ...
             * }
             *
             * New code:
             * //FluidStates can support lilypads if the block here is less than 1 block tall
             * if (plantable instanceof BlockBush && Hooks.canSustainLily(((BlockBush)plantable).canSustainBush(state), plantable, state, world, pos))
             * {
             *     ...
             * }
             */
            if(checkMethod(insn, obfuscated ? "func_185514_i" : "canSustainBush")) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 5));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, 2));
                list.add(new VarInsnNode(ALOAD, 3));
                list.add(genMethodNode("canSustainLily", "(ZLnet/minecraftforge/common/IPlantable;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
                instructions.insert(insn, list);
            }
            /*
             * canSustainPlant: (changes are around line 2075)
             * Old code:
             * boolean hasWater = (world.getBlockState(pos.east()).getMaterial() == Material.WATER ||
             *                     world.getBlockState(pos.west()).getMaterial() == Material.WATER ||
             *                     world.getBlockState(pos.north()).getMaterial() == Material.WATER ||
             *                     world.getBlockState(pos.south()).getMaterial() == Material.WATER);
             *
             * New code:
             * //check for FluidStates
             * boolean hasWater = (FluidloggedUtils.getFluidOrReal(world, pos.east()).getMaterial() == Material.WATER ||
             *                     FluidloggedUtils.getFluidOrReal(world, pos.west()).getMaterial() == Material.WATER ||
             *                     FluidloggedUtils.getFluidOrReal(world, pos.north()).getMaterial() == Material.WATER ||
             *                     FluidloggedUtils.getFluidOrReal(world, pos.south()).getMaterial() == Material.WATER);
             */
            else if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
            }
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canSustainLily(boolean canSustainBush, @Nonnull IPlantable plantable, @Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            if(canSustainBush) return true;
            else if(!(plantable instanceof BlockLilyPad)) return false;
            else return state.getBoundingBox(world, pos).maxY < 1
                        && FluidState.get(world, pos).getMaterial() == Material.WATER;
        }
    }
}
