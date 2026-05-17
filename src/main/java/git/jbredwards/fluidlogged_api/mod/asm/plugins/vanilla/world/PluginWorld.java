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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import atomicstryker.dynamiclights.client.DynamicLights;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.ICubeData;
import git.jbredwards.fluidlogged_api.api.world.IWorldProvider;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigFluidBox;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IWaterHeight;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.*;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;

/**
 * corrects a lot of FluidState related interactions
 * @author jbred
 *
 */
public final class PluginWorld implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        //setBlockState
        if(checkMethod(method, obfuscated ? "func_180501_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"))
            return 1;

        //destroyBlock
        else if(checkMethod(method, obfuscated ? "func_175655_b" : "destroyBlock", null))
            return 2;

        //neighborChanged
        else if(checkMethod(method, obfuscated ? "func_190524_a" : "neighborChanged", null))
            return 3;

        //isMaterialInBB
        else if(checkMethod(method, obfuscated ? "func_72875_a" : "isMaterialInBB", null)) {
            return 5;
        }

        //changes some methods to use FluidloggedUtils#getFluidOrReal
        else if(checkMethod(method, obfuscated ? "func_72953_d" : "containsAnyLiquid", null)
        || checkMethod(method, obfuscated ? "func_175696_F" : "isWater", null))
            return 6;

        //isFlammableWithin, fix bug with lava level
        else if(method.name.equals(obfuscated ? "func_147470_e" : "isFlammableWithin")) return 7;

        //fix neighbor brightness related bugs
        else if(checkMethod(method, obfuscated ? "func_175721_c" : "getLight", "(Lnet/minecraft/util/math/BlockPos;Z)I")
        || method.name.equals(obfuscated ? "func_175705_a" : "getLightFromNeighborsFor"))
            return 8;

        //allow FluidStates to output a redstone signal
        else if(method.name.equals(obfuscated ? "func_175651_c" : "getRedstonePower")) return 10;
        else if(checkMethod(method, obfuscated ? "func_175627_a" : "getStrongPower", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I"))
            return 11;

        else if(method.name.equals(obfuscated ? "func_180500_c" : "checkLightFor")) return 9;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //setBlockState
        if(index == 1) {
            /*
             * setBlockState: (changes are around line 409)
             * Old code:
             * IBlockState oldState = getBlockState(pos);
             *
             * New code:
             * //optimize by calling from already cached chunk value, cause why not
             * IBlockState oldState = chunk.getBlockState(pos);
             */
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                if(obfuscated) ((MethodInsnNode)insn).name = "func_177435_g";
                ((MethodInsnNode)insn).owner = "net/minecraft/world/chunk/Chunk";
                ((VarInsnNode)getPrevious(insn, 2)).var = findLocal(method, "chunk", "Lnet/minecraft/world/chunk/Chunk;").index;
            }
            /*
             * setBlockState: (changes are around lines 410 & 411)
             * Old code:
             * int oldLight = oldState.getLightValue(this, pos);
             * int oldOpacity = oldState.getLightOpacity(this, pos);
             *
             * New code:
             * //cache FluidState light levels
             * int oldLight = Hooks.getLightValue(oldState, this, pos, chunk);
             * int oldOpacity = Hooks.getLightOpacity(oldState, this, pos, chunk);
             */
            else if(checkMethod(insn, "getLightValue") || checkMethod(insn, "getLightOpacity")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, findLocal(method, "chunk", "Lnet/minecraft/world/chunk/Chunk;").index));
                instructions.insertBefore(insn, genMethodNode(((MethodInsnNode)insn).name, "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
                instructions.remove(insn);
            }
            /*
             * setBlockState: (changes are around line 413)
             * Old code:
             * IBlockState iblockstate = chunk.setBlockState(pos, newState);
             *
             * New code:
             * //remove FluidState here if the new state can't be fluidlogged, or move fluid block here to FluidState
             * IBlockState iblockstate = chunk.setBlockState(pos, newState);
             * Hooks.handleOldFluidState(this, pos, chunk, oldState, newState, iblockstate, flags);
             */
            else if(checkMethod(insn, obfuscated ? "func_177436_a" : "setBlockState")) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, findLocal(method, "chunk", "Lnet/minecraft/world/chunk/Chunk;").index));
                list.add(new VarInsnNode(ALOAD, findLocal(method, "oldState", "Lnet/minecraft/block/state/IBlockState;").index));
                list.add(new VarInsnNode(ALOAD, 2));
                list.add(new VarInsnNode(ALOAD, findLocal(method, "iblockstate", "Lnet/minecraft/block/state/IBlockState;").index));
                list.add(new VarInsnNode(ILOAD, 3));
                list.add(genMethodNode("handleOldFluidState", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/state/IBlockState;I)V"));
                instructions.insert(insn.getNext(), list);
            }

            //end method transform
            else return checkMethod(insn, "markAndNotifyBlock");
        }
        /*
         * destroyBlock: (changes are around line 492):
         * Old code:
         * return this.setBlockState(pos, Blocks.AIR.getDefaultState(), 3);
         *
         * New code:
         * //replace block here with FluidState here instead of air
         * return this.setBlockToAir(pos);
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_175698_g" : "setBlockToAir", "(Lnet/minecraft/util/math/BlockPos;)Z", false));
            removeFrom(instructions, insn, -3);
            return true;
        }
        //neighborChanged
        else if(index == 3) {
            /*
             * neighborChanged: (changes are around line 634)
             * Old code:
             * IBlockState iblockstate = this.getBlockState(pos);
             *
             * New code:
             * //save chunk for later use
             * Chunk chunk = this.getChunk(pos);
             * IBlockState iblockstate = chunk.getBlockState(pos);
             */
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_175726_f" : "getChunk", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", false));
                list.add(new VarInsnNode(ASTORE, 15));
                list.add(new VarInsnNode(ALOAD, 15));
                instructions.insertBefore(getPrevious(insn, 2), list);
                //change getBlockState method
                instructions.remove(getPrevious(insn, 2));
                if(obfuscated) ((MethodInsnNode)insn).name = "func_177435_g";
                ((MethodInsnNode)insn).owner = "net/minecraft/world/chunk/Chunk";
            }
            /*
             * neighborChanged: (changes are around line 638)
             * Old code:
             * iblockstate.neighborChanged(this, pos, blockIn, fromPos);
             *
             * New code:
             * //update FluidStates
             * iblockstate.neighborChanged(this, pos, blockIn, fromPos);
             * Hooks.fluidNeighborChanged(this, pos, blockIn, fromPos, chunk);
             */
            else if(checkMethod(insn, obfuscated ? "func_189546_a" : "neighborChanged", null)) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, 2));
                list.add(new VarInsnNode(ALOAD, 3));
                list.add(new VarInsnNode(ALOAD, 15));
                list.add(genMethodNode("fluidNeighborChanged", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)V"));

                instructions.insert(insn, list);
                return true;
            }
        }
        /*
         * isMaterialInBB: (changes are around line 2506)
         * Old code:
         * return false;
         *
         * New code:
         * //check FluidStates
         * return Hooks.isMaterialInFluidBB(this, bb, materialIn, j2, k2, l2, i3, j3, k3);
         */
        else if(index == 5 && insn.getOpcode() == ICONST_0) {
            final InsnList list = new InsnList();
            //params
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            //aabb positions
            list.add(new VarInsnNode(ILOAD, 3));
            list.add(new VarInsnNode(ILOAD, 4));
            list.add(new VarInsnNode(ILOAD, 5));
            list.add(new VarInsnNode(ILOAD, 6));
            list.add(new VarInsnNode(ILOAD, 7));
            list.add(new VarInsnNode(ILOAD, 8));
            //adds new code
            list.add(genMethodNode("isMaterialInFluidBB", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;IIIIII)Z"));

            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        //changes some methods to use FluidloggedUtils#getFluidOrReal
        else if(index == 6 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * isFlammableWithin: (changes are around line 2377)
         * Old code:
         * Block block = this.getBlockState(blockpos$pooledmutableblockpos.setPos(l3, i4, j4)).getBlock();
         *
         * New code:
         * //account for FluidStates
         * Block block = Hooks.isFlammableFluidWithin(this.getBlockState(blockpos$pooledmutableblockpos.setPos(l3, i4, j4)), this, blockpos$pooledmutableblockpos, bb).getBlock();
         */
        else if(index == 7 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, findLocal(method, "blockpos$pooledmutableblockpos", "Lnet/minecraft/util/math/BlockPos$PooledMutableBlockPos;").index));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(genMethodNode("isFlammableFluidWithin", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            return true;
        }
        /*
         * getLight & getLightFromNeighborsFor: (changes around lines 768 & 899):
         * Old code:
         * if (checkNeighbors && this.getBlockState(pos).useNeighborBrightness())
         * {
         *     ...
         * }
         *
         * New code:
         * //fix neighbor brightness related bugs
         * if (checkNeighbors && Hooks.useNeighborBrightness(this, pos))
         * {
         *     ...
         * }
         */
        else if(index == 8 && checkMethod(insn, obfuscated ? "func_185916_f" : "useNeighborBrightness")) {
            instructions.insert(insn, genMethodNode("useNeighborBrightness", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
            removeFrom(instructions, insn, -1);
            return true;
        }
        /*
         * checkLightFor: (changes are around line 3165)
         * Old code:
         * IBlockState bs = this.getBlockState(blockpos$pooledmutableblockpos);
         * int i7 = Math.max(1, bs.getBlock().getLightOpacity(bs, this, blockpos$pooledmutableblockpos));
         *
         * New code:
         * //use forge-added opacity getter
         * IBlockState bs = null;
         * int i7 = Math.max(1, this.getBlockLightOpacity(blockpos$pooledmutableblockpos));
         */
        else if(index == 9) {
            //don't collect block state here, it's unused
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insert(insn, new InsnNode(ACONST_NULL));
                removeFrom(instructions, insn, -2);
            }
            else if(checkMethod(insn, "getLightOpacity")) {
                removeFrom(instructions, getPrevious(insn, 3), -2);
                instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", "getBlockLightOpacity", "(Lnet/minecraft/util/math/BlockPos;)I", false));
                instructions.remove(insn);
                return true;
            }
        }
        /*
         * getRedstonePower: (changes are around line 3558)
         * Old code:
         * return ... iblockstate1.getWeakPower(this, pos, facing);
         *
         * New code:
         * // Allow FluidStates to output a redstone signal
         * return ... Hooks.getRedstonePowerHook(iblockstate1.getWeakPower(this, pos, facing), iblockstate1, this, pos, facing);
         */
        else if(index == 10 && checkMethod(insn.getPrevious(), obfuscated ? "func_185911_a" : "getWeakPower")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 3));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, genMethodNode("getRedstonePowerHook", "(ILnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I"));
            return true;
        }
        /*
         * getStrongPower: (changes are around line 3487)
         * Old code:
         * return this.getBlockState(pos).getStrongPower(this, pos, direction);
         *
         * New code:
         * // Allow FluidStates to output a redstone signal
         * return Hooks.getStrongPowerHook(this.getBlockState(pos).getStrongPower(this, pos, direction), this, pos, direction);
         */
        else if(index == 11 && checkMethod(insn.getPrevious(), obfuscated ? "func_185893_b" : "getStrongPower")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, genMethodNode("getStrongPowerHook", "(ILnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I"));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IWorldChunkProvider");
        addMethod(classNode, "getWorld", "()Lnet/minecraft/world/World;", null, null, generator -> generator.visitVarInsn(ALOAD, 0));
        /*
         * handleMaterialAcceleration:
         * New code:
         * // Account for FluidStates when calculating material acceleration
         * public boolean handleMaterialAcceleration(AxisAlignedBB bb, Material materialIn, Entity entityIn)
         * {
         *     return Hooks.handleMaterialAcceleration(this, bb, materialIn, entityIn);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_72918_a" : "handleMaterialAcceleration"),
            "handleMaterialAcceleration", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;Lnet/minecraft/entity/Entity;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );
        /*
         * rayTraceBlocks:
         * New code:
         * //ray traces now include fluidlogged fluid blocks
         * @Nullable
         * public RayTraceResult rayTraceBlocks(Vec3d vec31, Vec3d vec32, boolean stopOnLiquid, boolean ignoreBlockWithoutBoundingBox, boolean returnLastUncollidableBlock)
         * {
         *     return Hooks.rayTraceBlocks(this, vec31, vec32, stopOnLiquid, ignoreBlockWithoutBoundingBox, returnLastUncollidableBlock);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_147447_a" : "rayTraceBlocks", "(Lnet/minecraft/util/math/Vec3d;Lnet/minecraft/util/math/Vec3d;ZZZ)Lnet/minecraft/util/math/RayTraceResult;"),
            "rayTraceBlocks", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/Vec3d;Lnet/minecraft/util/math/Vec3d;ZZZ)Lnet/minecraft/util/math/RayTraceResult;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
                generator.visitVarInsn(ILOAD, 5);
            }
        );
        /*
         * getRawLight:
         * New code:
         * //account for FluidStates when calculating raw light & opacity values
         * private int getRawLight(BlockPos pos, EnumSkyBlock lightType)
         * {
         *     return Hooks.getRawLight(this, pos, lightType);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_175638_a" : "getRawLight"),
            "getRawLight", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/EnumSkyBlock;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        /*
         * isAirBlock:
         * New code:
         * //increase performance
         * public boolean isAirBlock(BlockPos pos)
         * {
         *     IBlockState state = this.getBlockState(pos);
         *     return state.getBlock().isAir(state, this, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_175623_d" : "isAirBlock"), null, null, generator -> {
            final int stateVar = generator.newLocal(Type.getType("Lnet/minecraft/block/state/IBlockState;"));
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false);
            generator.visitVarInsn(ASTORE, stateVar);

            generator.visitVarInsn(ALOAD, stateVar);
            generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraft/block/state/IBlockState", obfuscated ? "func_177230_c" : "getBlock", "()Lnet/minecraft/block/Block;", true);
            generator.visitVarInsn(ALOAD, stateVar);
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/block/Block", "isAir", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", false);
        });

        return true;
    }

    // largely copied from generated bytecode from PluginWorld$Hooks.setBlockToAir(),
    // to get around forge falsely labeling it as the cause for cascading world gen.
    @Nonnull
    @Override
    public byte[] transform(@Nonnull final byte[] basicClass, final boolean obfuscated) {
        @Nonnull final ClassNode classNode = new ClassNode();
        new ClassReader(basicClass).accept(classNode, 0);
        /*
         * setBlockToAir:
         * New code:
         * // Replace block here with FluidState here instead of air
         * @ASMOverwrite
         * public boolean setBlockToAir(BlockPos pos)
         * {
         *     Chunk chunk = this.getChunk(pos);
         *     if(this.isRemote && FluidloggedUtils.isFluid(chunk.getBlockState(pos)) return false;
         *     else return this.setBlockState(pos, FluidState.getFromProvider(chunk, pos).toFlowing().getState(), 35);
         * }
         */
        for(@Nonnull final MethodNode mv : classNode.methods) if(mv.name.equals(obfuscated ? "func_175698_g" : "setBlockToAir")) {
            overrideMethod(classNode, mv, null, null, generator -> {});
            mv.instructions.clear();
            mv.visitCode();
            @Nonnull final Label l0 = new Label();
            mv.visitLabel(l0);
            // mv.visitLineNumber(881, l0);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_175726_f" : "getChunk", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", false);
            mv.visitVarInsn(ASTORE, 2);
            @Nonnull final Label l1 = new Label();
            mv.visitLabel(l1);
            // mv.visitLineNumber(882, l1);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, "net/minecraft/world/World", obfuscated ? "field_72995_K" : "isRemote", "Z");
            @Nonnull final Label l2 = new Label();
            mv.visitJumpInsn(IFEQ, l2);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/chunk/Chunk", obfuscated ? "func_177435_g" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false);
            mv.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "isFluid", "(Lnet/minecraft/block/state/IBlockState;)Z", false);
            mv.visitJumpInsn(IFEQ, l2);
            mv.visitInsn(ICONST_0);
            mv.visitInsn(IRETURN);
            mv.visitLabel(l2);
            // mv.visitLineNumber(883, l2);
            mv.visitFrame(F_APPEND, 1, new Object[] {"net/minecraft/world/chunk/Chunk"}, 0, null);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getFromProvider", "(Lnet/minecraftforge/common/capabilities/ICapabilityProvider;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", false);
            mv.visitMethodInsn(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "toFlowing", "()Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", false);
            mv.visitMethodInsn(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getState", "()Lnet/minecraft/block/state/IBlockState;", false);
            mv.visitIntInsn(BIPUSH, 35);
            mv.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_180501_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z", false);
            mv.visitInsn(IRETURN);
            @Nonnull final Label l3 = new Label();
            mv.visitLabel(l3);
            mv.visitLocalVariable("world", "Lnet/minecraft/world/World;", null, l0, l3, 0);
            mv.visitLocalVariable("pos", "Lnet/minecraft/util/math/BlockPos;", null, l0, l3, 1);
            mv.visitLocalVariable("chunk", "Lnet/minecraft/world/chunk/Chunk;", null, l1, l3, 2);
            mv.visitEnd();
            break;
        }

        // apply this without computing frames, and run the rest of the transform with computed frames
        @Nonnull final ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        classNode.accept(writer);
        return IASMPlugin.super.transform(writer.toByteArray(), obfuscated);
    }

    @Override
    public boolean addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) {
        if(index == 3) {
            method.localVariables.add(new LocalVariableNode("chunk", "Lnet/minecraft/world/chunk/Chunk;", null, start, end, 15));
            return true;
        }

        return false;
    }

    @Override
    public boolean recalcFrames(final boolean obfuscated) { return true; }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void fluidNeighborChanged(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Block blockIn, @Nonnull BlockPos fromPos, @Nonnull Chunk chunk) {
            FluidState.getFromProvider(chunk, pos).getState().neighborChanged(world, pos, blockIn, fromPos);
        }

        public static int getLightOpacity(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return PluginChunk.Hooks.getFluidLightOpacity(state, world, pos, chunk);
        }

        public static int getLightValue(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return PluginChunk.Hooks.getFluidLightValue(state, world, pos, chunk);
        }

        public static int getRawLight(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumSkyBlock lightType) {
            if(lightType == EnumSkyBlock.SKY && world.canSeeSky(pos)) return 15;
            final ICubeData cube = ICubeData.get(world, pos);
            final IBlockState state = cube.getBlockState(pos);
            final IBlockState fluidState = cube.getFluidState(pos).getState();
            int light = lightType == EnumSkyBlock.SKY ? 0 : FluidloggedAPI.isDynamicLights
                    ? DLHooks.getLightValue(state, world, pos, fluidState)
                    : Math.max(state.getLightValue(world, pos), fluidState.getLightValue(world, pos));
            int opacity = Math.max(Math.max(state.getLightOpacity(world, pos), fluidState.getLightOpacity(world, pos)), 1);
            if(opacity >= 15) return light; // Forge: fix MC-119932
            else if(light >= 14) return light;

            for(EnumFacing facing : EnumFacing.VALUES) {
                final BlockPos offset = pos.offset(facing);
                final int neighborLight = world.getLightFor(lightType, offset) - opacity;

                if(neighborLight > light) light = neighborLight;
                if(light >= 14) return light;
            }

            return light;
        }

        public static int getRedstonePowerHook(final int original, @Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing direction) {
            if(!FluidloggedAPIConfig.fluidStateEmitRedstone || original >= 15 || FluidloggedUtils.isFluid(state)) return original;

            @Nonnull final IBlockState fluidState = FluidState.get(world, pos).getState();
            if(fluidState.getBlock().shouldCheckWeakPower(fluidState, world, pos, direction)) return world.getStrongPower(pos);

            else return Math.max(original, fluidState.getWeakPower(world, pos, direction));
        }

        public static int getStrongPowerHook(final int original, @Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final EnumFacing direction) {
            return !FluidloggedAPIConfig.fluidStateEmitRedstone || original >= 15 ? original : Math.max(original, FluidState.get(access, pos).getState().getStrongPower(access, pos, direction));
        }

        public static boolean handleMaterialAcceleration(@Nonnull final World world, @Nonnull final AxisAlignedBB bb, @Nonnull final Material material, @Nonnull final Entity entity) {
            final int minX = MathHelper.floor(bb.minX), minY = MathHelper.floor(bb.minY), minZ = MathHelper.floor(bb.minZ), maxX = MathHelper.ceil(bb.maxX), maxY = MathHelper.ceil(bb.maxY), maxZ = MathHelper.ceil(bb.maxZ);
            final boolean checkExtendedStates = FluidloggedAPIConfig.fancyFluidEntityCollision.test(bb, entity);
            final int
                    minCacheX = checkExtendedStates ? minX - 1 : minX,
                    minCacheY = checkExtendedStates ? minY - 1 : minY,
                    minCacheZ = checkExtendedStates ? minZ - 1 : minZ,
                    maxCacheX = checkExtendedStates ? maxX : maxX - 1,
                    maxCacheY = checkExtendedStates ? maxY : maxY - 1,
                    maxCacheZ = checkExtendedStates ? maxZ : maxZ - 1;

            if(!world.isAreaLoaded(minCacheX, minCacheY, minCacheZ, maxCacheX, maxCacheY, maxCacheZ, true)) return false;

            @Nonnull final IWaterHeight waterHeight = (IWaterHeight)entity;
            @Nonnull final BlockPos.PooledMutableBlockPos pos = BlockPos.PooledMutableBlockPos.retain();
            if(material == Material.WATER) FluidCollisionHandler.cacheHeight.set(waterHeight);

            @Nonnull final FluidCache cache = new FluidCache(world, minCacheX, maxCacheX, minCacheY, maxCacheY, minCacheZ, maxCacheZ);
            @Nonnull Vec3d vec = Vec3d.ZERO;

            waterHeight.setBox(null);
            final boolean pushEntity = entity.isPushedByWater();

            boolean anyFound = false;
            int total = 0;

            for(int x = minX; x < maxX; x++) {
                for(int z = minZ; z < maxZ; z++) {
                    for(int y = minY; y < maxY; y++) {
                        @Nonnull final IBlockState state = cache.getBlockState(pos.setPos(x, y, z));
                        @Nullable Boolean result = state.getBlock().isEntityInsideMaterial(cache, pos, state, entity, maxY, material, false);
                        if(result != null) {
                            if(result) {
                                anyFound = true;
                                final boolean isFluid = FluidloggedUtils.isFluid(state);
                                if(!isFluid) waterHeight.setBox(new IConfigFluidBox.HeightBox(0, 1));
                                if(pushEntity) {
                                    // vec = state.getBlock().modifyAcceleration(world, pos, entity, vec);
                                    @Nonnull Vec3d fluidVec = state.getBlock().modifyAcceleration(world, pos, entity, Vec3d.ZERO);
                                    if(isFluid) {
                                        final double height = FluidState.of(state).getActualHeight(cache, pos);
                                        if(height < 0.4) fluidVec = fluidVec.scale(height);
                                    }

                                    total++;
                                    vec = vec.add(fluidVec);
                                }
                            }

                            continue;
                        }

                        @Nonnull final FluidState fluidState = FluidState.get(cache, pos);
                        result = fluidState.getBlock().isEntityInsideMaterial(cache, pos, fluidState.getState(), entity, maxY, material, false);
                        if(result != null) {
                            if(result) {
                                anyFound = true;
                                if(pushEntity) {
                                    // vec = fluidState.getBlock().modifyAcceleration(world, pos, entity, vec);
                                    @Nonnull Vec3d fluidVec = fluidState.getBlock().modifyAcceleration(world, pos, entity, Vec3d.ZERO);
                                    final double height = fluidState.getActualHeight(cache, pos);
                                    if(height < 0.4) fluidVec = fluidVec.scale(height);

                                    total++;
                                    vec = vec.add(fluidVec);
                                }
                            }

                            continue;
                        }

                        // Fluidlogged API makes fluid blocks use Block::isEntityInsideMaterial, the code below exists only for "fake fluid blocks" (like BOP coral)
                        @Nonnull final IBlockState here = fluidState == FluidState.EMPTY ? state : fluidState.getState();
                        if(here.getMaterial() == material) {
                            waterHeight.setBox(new IConfigFluidBox.HeightBox(0, 1));
                            anyFound = true;
                            if(pushEntity) {
                                // vec = here.getBlock().modifyAcceleration(world, pos, entity, vec);
                                total++;
                                vec = vec.add(here.getBlock().modifyAcceleration(world, pos, entity, Vec3d.ZERO));
                            }
                        }
                    }
                }
            }

            pos.release();
            if(vec.length() > 0) {
                if(total > 0) vec = vec.scale(1d / total);
                if(!(entity instanceof EntityPlayer)) vec = vec.normalize();

                entity.motionX += vec.x * 0.014;
                entity.motionY += vec.y * 0.014;
                entity.motionZ += vec.z * 0.014;
            }

            if(material == Material.WATER) FluidCollisionHandler.cacheHeight.set(null);
            return anyFound;
        }

        public static void handleOldFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk, @Nonnull IBlockState oldState, @Nonnull IBlockState newState, @Nullable IBlockState iblockstate, int blockFlags) {
            if(iblockstate != null) {
                final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
                final boolean notifyFluid = (blockFlags & Constants.BlockFlags.NOTIFY_NEIGHBORS) != 0;
                blockFlags &=~ Constants.BlockFlags.NOTIFY_NEIGHBORS; // don't notify neighbors twice

                // notify neighboring FluidStates of any non-fluid state changes, if the block itself won't already
                if(!world.isRemote && !notifyFluid && chunk.isPopulated() && !FluidloggedUtils.isFluid(newState)) {
                    @Nonnull final BlockPos.MutableBlockPos mutablePos = new BlockPos.MutableBlockPos();
                    @Nonnull final Chunk[][] chunks = new Chunk[2][2];

                    fluidState.getState().neighborChanged(world, pos, newState.getBlock(), pos);
                    for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
                        mutablePos.setPos(pos.getX() + side.getXOffset(), pos.getY(), pos.getZ() + side.getZOffset());
                        final int x = mutablePos.getX() >> 4, z = mutablePos.getZ() >> 4;

                        @Nonnull final Chunk neighborChunk = chunks[x & 1][z & 1] == null ? chunks[x & 1][z & 1] = world.getChunk(x, z) : chunks[x & 1][z & 1];
                        if(neighborChunk.isPopulated()) FluidloggedUtils.getFluidState(neighborChunk, mutablePos).getState().neighborChanged(world, mutablePos, newState.getBlock(), pos);
                    }
                }

                // this mod adds two special flags:
                // 32: (x | 32, example: Constants.BlockFlags.DEFAULT | 32) that removes any FluidState here
                // 64: (x | 64, example: Constants.BlockFlags.DEFAULT | 64) that ignores FluidState.removeOnBlockChange
                if((blockFlags & 64) == 0 && ((blockFlags & 32) != 0 || !(newState.getBlock() instanceof IFluidloggable) && FluidState.removeOnBlockChange.get() != null)) {
                    if(fluidState != FluidState.EMPTY) FluidloggedUtils.setFluidState(world, pos, newState, FluidState.EMPTY, false, blockFlags);
                }

                // if the new state isn't fluidloggable, remove the FluidState here
                else if(fluidState.getBlock() instanceof IFluidloggableFluid) {
                    if(!((IFluidloggableFluid)fluidState.getBlock()).isStateFluidloggable(newState, world, pos, fluidState)) FluidloggedUtils.setFluidState(world, pos, newState, FluidState.EMPTY, false, blockFlags);
                    else if(!world.isRemote && (notifyFluid || chunk.isPopulated())) fluidState.getState().neighborChanged(world, pos, newState.getBlock(), pos);
                }

                // save oldState as FluidState if possible
                else if(oldState.getBlock() instanceof IFluidloggableFluid) {
                    final IFluidloggableFluid handler = (IFluidloggableFluid)oldState.getBlock();
                    final FluidState oldFluidState = FluidState.of(oldState);

                    if(handler.isFluidloggableFluid(oldFluidState) && handler.isStateFluidloggable(newState, world, pos, oldFluidState)) {
                        FluidloggedUtils.setFluidState(world, pos, newState, oldFluidState, false, blockFlags);
                        if(!world.isRemote) oldFluidState.getState().neighborChanged(world, pos, newState.getBlock(), pos);
                    }
                }
            }
        }

        @Nonnull
        public static IBlockState isFlammableFluidWithin(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb) {
            if(state.getBlock() == Blocks.FIRE) return state;
            else if(FluidCollisionHandler.isAABBInsideMaterial(state, world, pos, bb, Material.LAVA)) return Blocks.FIRE.getDefaultState();
            else if(state.getBlock().isAir(state, world, pos) || FluidloggedUtils.isFluid(state)) return Blocks.AIR.getDefaultState();

            final FluidState fluidState = FluidState.get(world, pos); //handle possible lava FluidState
            return FluidCollisionHandler.isAABBInsideMaterial(fluidState.getState(), world, pos, bb, Material.LAVA) ? Blocks.FIRE.getDefaultState() : state;
        }

        public static boolean isMaterialInFluidBB(@Nonnull World world, @Nonnull AxisAlignedBB bb, @Nonnull Material materialIn, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
            for(int x = minX; x < maxX; ++x) {
                for(int y = minY; y < maxY; ++y) {
                    for(int z = minZ; z < maxZ; ++z) {
                        final BlockPos pos = new BlockPos(x, y, z);
                        if(FluidCollisionHandler.isAABBInsideMaterial(FluidState.get(world, pos).getState(), world, pos, bb, materialIn)) return true;
                    }
                }
            }

            return false;
        }

        @SuppressWarnings("ConstantConditions")
        @Nullable
        public static RayTraceResult rayTraceBlocks(@Nonnull World world, @Nonnull Vec3d vec, @Nonnull Vec3d end, boolean stopOnLiquid, boolean ignoreBlockWithoutBoundingBox, boolean returnLastUncollidableBlock) {
            if(Double.isNaN(vec.x) || Double.isNaN(vec.y) || Double.isNaN(vec.z) || Double.isNaN(end.x) || Double.isNaN(end.y) || Double.isNaN(end.z))
                return null;

            final int endX = MathHelper.floor(end.x);
            final int endY = MathHelper.floor(end.y);
            final int endZ = MathHelper.floor(end.z);
            int prevX = MathHelper.floor(vec.x);
            int prevY = MathHelper.floor(vec.y);
            int prevZ = MathHelper.floor(vec.z);

            BlockPos pos = new BlockPos(prevX, prevY, prevZ);
            RayTraceResult result;
            Chunk lastChunk = world.getChunk(pos); // cache the last accessed chunk to boost performance
            boolean isOutOfWorld = world.isOutsideBuildHeight(pos);

            //check FluidState
            FluidState fluidState = isOutOfWorld ? FluidState.EMPTY : FluidState.getFromProvider(lastChunk, pos);
            if(fluidState.getBlock().canCollideCheck(fluidState.getState(), stopOnLiquid) && (stopOnLiquid && !fluidState.isEmpty() || !ignoreBlockWithoutBoundingBox || fluidState.getState().getCollisionBoundingBox(world, pos) != Block.NULL_AABB)) {
                result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                if(result != null) { return result; }
            }

            IBlockState state = isOutOfWorld ? Blocks.AIR.getDefaultState() : lastChunk.getBlockState(pos);
            if(state.getBlock().canCollideCheck(state, stopOnLiquid) && (stopOnLiquid && FluidloggedUtils.isFluid(state) || !ignoreBlockWithoutBoundingBox || state.getCollisionBoundingBox(world, pos) != Block.NULL_AABB)) {
                result = state.collisionRayTrace(world, pos, vec, end);
                if(result != null) { return result; }
            }

            RayTraceResult lastResult = FluidloggedAPI.isChiseledMe ? new RayTraceResult(RayTraceResult.Type.MISS, end, EnumFacing.DOWN, pos) : null;
            for(int i = 200; i-- >= 0;) {
                if(Double.isNaN(vec.x) || Double.isNaN(vec.y) || Double.isNaN(vec.z))
                    return null;

                if(prevX == endX && prevY == endY && prevZ == endZ)
                    return returnLastUncollidableBlock ? lastResult : null;

                boolean flagX = true, flagY = true, flagZ = true;
                double x = 999, y = 999, z = 999;

                if(endX > prevX) x = prevX + 1;
                else if(endX < prevX) x = prevX;
                else flagX = false;

                if(endY > prevY) y = prevY + 1;
                else if(endY < prevY) y = prevY;
                else flagY = false;

                if(endZ > prevZ) z = prevZ + 1;
                else if(endZ < prevZ) z = prevZ;
                else flagZ = false;

                double coveredX = 999, coveredY = 999, coveredZ = 999;
                double distX = end.x - vec.x;
                double distY = end.y - vec.y;
                double distZ = end.z - vec.z;

                if(flagX) coveredX = (x - vec.x) / distX;
                if(flagY) coveredY = (y - vec.y) / distY;
                if(flagZ) coveredZ = (z - vec.z) / distZ;

                if(coveredX == -0) coveredX = -1.0E-4;
                if(coveredY == -0) coveredY = -1.0E-4;
                if(coveredZ == -0) coveredZ = -1.0E-4;

                //the general direction of the trace
                EnumFacing facing;

                if(coveredX < coveredY && coveredX < coveredZ) {
                    facing = endX > prevX ? EnumFacing.WEST : EnumFacing.EAST;
                    vec = new Vec3d(x, vec.y + distY * coveredX, vec.z + distZ * coveredX);
                }

                else if(coveredY < coveredZ) {
                    facing = endY > prevY ? EnumFacing.DOWN : EnumFacing.UP;
                    vec = new Vec3d(vec.x + distX * coveredY, y, vec.z + distZ * coveredY);
                }

                else {
                    facing = endZ > prevZ ? EnumFacing.NORTH : EnumFacing.SOUTH;
                    vec = new Vec3d(vec.x + distX * coveredZ, vec.y + distY * coveredZ, z);
                }

                prevX = MathHelper.floor(vec.x) - (facing == EnumFacing.EAST  ? 1 : 0);
                prevY = MathHelper.floor(vec.y) - (facing == EnumFacing.UP    ? 1 : 0);
                prevZ = MathHelper.floor(vec.z) - (facing == EnumFacing.SOUTH ? 1 : 0);

                pos = new BlockPos(prevX, prevY, prevZ);
                isOutOfWorld = world.isOutsideBuildHeight(pos);
                if(!lastChunk.isAtLocation(prevX >> 4, prevZ >> 4)) lastChunk = world.getChunk(pos);

                //check FluidState
                fluidState = isOutOfWorld ? FluidState.EMPTY : FluidState.getFromProvider(lastChunk, pos);
                if(!ignoreBlockWithoutBoundingBox || stopOnLiquid && !fluidState.isEmpty() || fluidState.getState().getCollisionBoundingBox(world, pos) != Block.NULL_AABB) {
                    if(fluidState.getBlock().canCollideCheck(fluidState.getState(), stopOnLiquid)) {
                        result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                        if(result != null) return result;
                    }

                    else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
                }

                state = isOutOfWorld ? Blocks.AIR.getDefaultState() : lastChunk.getBlockState(pos);
                if(!ignoreBlockWithoutBoundingBox || state.getMaterial() == Material.PORTAL || stopOnLiquid && FluidloggedUtils.isFluid(state) || state.getCollisionBoundingBox(world, pos) != Block.NULL_AABB) {
                    if(state.getBlock().canCollideCheck(state, stopOnLiquid)) {
                        result = state.collisionRayTrace(world, pos, vec, end);
                        if(result != null) return result;
                    }

                    else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
                }
            }

            return returnLastUncollidableBlock ? lastResult : null;
        }

        // helper
        public static boolean setBlockToAir(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState airState, final int blockFlags) {
            @Nonnull final ICubeData cube = ICubeData.get(world, pos);
            if(world.isRemote && FluidloggedUtils.isFluid(cube.getBlockState(pos))) return false; // prevents possible client desync
            else return world.setBlockState(pos, cube.getFluidState(pos).toFlowing().getState(), blockFlags | 32);
        }

        public static boolean useNeighborBrightness(@Nonnull World world, @Nonnull BlockPos pos) {
            return PluginChunkCache.Hooks.useNeighborBrightness(world, pos);
        }

        // ------------------------------------
        // Old redstone overwrites, now unused.
        // ------------------------------------

        @Deprecated
        public static int getRedstonePower(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing direction) {
            return IWorldProvider.getWorld(world).getRedstonePower(pos, direction);
        }

        @Deprecated
        public static int getRedstonePowerFromNeighbors(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
            return IWorldProvider.getWorld(world).getRedstonePowerFromNeighbors(pos);
        }

        @Deprecated
        public static int getStrongPower(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
            return IWorldProvider.getWorld(world).getStrongPower(pos);
        }

        @Deprecated
        public static int getStrongPower(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing direction) {
            return IWorldProvider.getWorld(world).getStrongPower(pos, direction);
        }

        @Deprecated
        public static boolean isBlockPowered(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
            return IWorldProvider.getWorld(world).isBlockPowered(pos);
        }
    }

    // hold Dynamic Lights methods in separate class to avoid crash
    public static final class DLHooks
    {
        public static int getLightValue(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState fluidState) {
            return Math.max(DynamicLights.getLightValue(state.getBlock(), state, world, pos), fluidState.getLightValue(world, pos));
        }
    }
}
