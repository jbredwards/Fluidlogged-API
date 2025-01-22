/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import atomicstryker.dynamiclights.client.DynamicLights;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
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
import net.minecraft.world.ChunkCache;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

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

        //setBlockToAir
        else if(checkMethod(method, obfuscated ? "func_175698_g" : "setBlockToAir", null))
            return 2;

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
        || checkMethod(method, obfuscated ? "func_147470_e" : "isFlammableWithin", null)
        || checkMethod(method, obfuscated ? "func_175696_F" : "isWater", null))
            return 6;

        //isFlammableWithin, fix bug with lava level
        else if(method.name.equals(obfuscated ? "func_147470_e" : "isFlammableWithin")) return 7;

        //fix neighbor brightness related bugs
        else if(checkMethod(method, obfuscated ? "func_175721_c" : "getLight", "(Lnet/minecraft/util/math/BlockPos;Z)I")
        || method.name.equals(obfuscated ? "func_175705_a" : "getLightFromNeighborsFor"))
            return 8;

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
         * setBlockToAir & destroyBlock: (changes are around lines 468 & 492):
         * Old code:
         * return this.setBlockState(pos, Blocks.AIR.getDefaultState(), 3);
         *
         * New code:
         * //replace block here with FluidState here instead of air
         * return Hooks.setBlockToAir(this, pos, Blocks.AIR.getDefaultState(), 3);
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
            instructions.insert(insn, genMethodNode("setBlockToAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
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
         * isFlammableWithin: (changes are around line 2379)
         * Old code:
         * if (block == Blocks.FIRE || block == Blocks.FLOWING_LAVA || block == Blocks.LAVA)
         * {
         *     ...
         * }
         *
         * New code:
         * //account for FluidStates
         * if (block == Blocks.FIRE || Hooks.isFlammableFluidWithin(block, this, blockpos$pooledmutableblockpos, bb))
         * {
         *     ...
         * }
         */
        else if(index == 7 && checkField(insn, obfuscated ? "field_150353_l" : "LAVA")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, findLocal(method, "blockpos$pooledmutableblockpos", "Lnet/minecraft/util/math/BlockPos$PooledMutableBlockPos;").index));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(genMethodNode("isFlammableFluidWithin", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Z"));
            instructions.insert(insn, list);
            removeFrom(instructions, insn, -3);
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
        /*
         * getStrongPower:
         * New code:
         * // Allow FluidStates to output a redstone signal
         * public int getStrongPower(BlockPos pos, EnumFacing direction)
         * {
         *     return Hooks.getStrongPower(this, pos, direction);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_175627_a" : "getStrongPower", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I"),
            "getStrongPower", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        /*
         * getStrongPower:
         * New code:
         * // Allow FluidStates to output a redstone signal
         * public int getStrongPower(BlockPos pos)
         * {
         *     return Hooks.getStrongPower(this, pos);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_175676_y" : "getStrongPower", "(Lnet/minecraft/util/math/BlockPos;)I"),
            "getStrongPower", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
            }
        );
        /*
         * getRedstonePower:
         * New code:
         * // Allow FluidStates to output a redstone signal
         * public int getRedstonePower(BlockPos pos, EnumFacing facing)
         * {
         *     return Hooks.getRedstonePower(this, pos, facing);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_175651_c" : "getRedstonePower"),
            "getRedstonePower", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        /*
         * getRedstonePowerFromNeighbors:
         * New code:
         * // Allow FluidStates to output a redstone signal
         * public int getRedstonePowerFromNeighbors(BlockPos pos)
         * {
         *     return Hooks.getRedstonePowerFromNeighbors(this, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_175687_A" : "getRedstonePowerFromNeighbors"),
            "getRedstonePowerFromNeighbors", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
            }
        );
        /*
         * isBlockPowered:
         * New code:
         * // Allow FluidStates to output a redstone signal
         * public boolean isBlockPowered(BlockPos pos)
         * {
         *     return Hooks.isBlockPowered(this, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_175640_z" : "isBlockPowered"),
            "isBlockPowered", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
            }
        );

        return true;
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
            final Chunk chunk = world.getChunk(pos);
            final IBlockState state = chunk.getBlockState(pos);
            final IBlockState fluidState = FluidState.getFromProvider(chunk, pos).getState();
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

        public static int getRedstonePower(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing direction) {
            @Nonnull final IBlockAccess access = world instanceof World ? new FluidCache(world, pos, 3, 3) : world;
            @Nonnull final IBlockState state = access.getBlockState(pos);

            if(state.getBlock().shouldCheckWeakPower(state, access, pos, direction)) return getStrongPower(access, pos);
            else if(FluidloggedAPIConfig.fixBadFluidMixing && !FluidloggedUtils.canFluidFlow(access, pos, state, direction.getOpposite())) return state.getWeakPower(access, pos, direction);

            @Nonnull final IBlockState fluidState = FluidState.get(access, pos).getState();
            if(fluidState.getBlock().shouldCheckWeakPower(fluidState, access, pos, direction)) return getStrongPower(access, pos);
            else return Math.max(state.getWeakPower(access, pos, direction), fluidState.getWeakPower(access, pos, direction));
        }

        public static int getRedstonePowerFromNeighbors(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
            @Nonnull final IBlockAccess access = world instanceof World ? new FluidCache(world, pos, 4, 4) : world;
            @Nonnull final FluidState fluidState = FluidState.get(access, pos);

            int currMax = fluidState.isEmpty() ? 0 : fluidState.getState().getWeakPower(access, pos, fluidState.getDownDensityFace());
            if(currMax >= 15) return currMax;

            else if((currMax = Math.max(currMax, getRedstonePower(access, pos.down(), EnumFacing.DOWN))) >= 15) return currMax;
            else if((currMax = Math.max(currMax, getRedstonePower(access, pos.up(), EnumFacing.UP))) >= 15) return currMax;
            else if((currMax = Math.max(currMax, getRedstonePower(access, pos.north(), EnumFacing.NORTH))) >= 15) return currMax;
            else if((currMax = Math.max(currMax, getRedstonePower(access, pos.south(), EnumFacing.SOUTH))) >= 15) return currMax;
            else if((currMax = Math.max(currMax, getRedstonePower(access, pos.west(), EnumFacing.WEST))) >= 15) return currMax;
            else return Math.max(currMax, getRedstonePower(access, pos.east(), EnumFacing.EAST));
        }

        public static int getStrongPower(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
            @Nonnull final IBlockAccess access = world instanceof World ? new FluidCache(world, pos, 2, 2) : world;
            @Nonnull final FluidState fluidState = FluidState.get(access, pos);

            int currMax = fluidState.isEmpty() ? 0 : fluidState.getState().getStrongPower(access, pos, fluidState.getDownDensityFace());
            if(currMax >= 15) return currMax;

            else if((currMax = Math.max(currMax, getStrongPower(access, pos.down(), EnumFacing.DOWN))) >= 15) return currMax;
            else if((currMax = Math.max(currMax, getStrongPower(access, pos.up(), EnumFacing.UP))) >= 15) return currMax;
            else if((currMax = Math.max(currMax, getStrongPower(access, pos.north(), EnumFacing.NORTH))) >= 15) return currMax;
            else if((currMax = Math.max(currMax, getStrongPower(access, pos.south(), EnumFacing.SOUTH))) >= 15) return currMax;
            else if((currMax = Math.max(currMax, getStrongPower(access, pos.west(), EnumFacing.WEST))) >= 15) return currMax;
            else return Math.max(currMax, getStrongPower(access, pos.east(), EnumFacing.EAST));
        }

        public static int getStrongPower(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing direction) {
            @Nonnull final IBlockAccess access = world instanceof World ? new FluidCache(world, pos, 1, 1) : world;
            @Nonnull final IBlockState state = access.getBlockState(pos);

            if(FluidloggedAPIConfig.fixBadFluidMixing && !FluidloggedUtils.canFluidFlow(access, pos, state, direction.getOpposite())) return state.getStrongPower(access, pos, direction);
            else return Math.max(state.getStrongPower(access, pos, direction), FluidState.get(access, pos).getState().getStrongPower(access, pos, direction));
        }

        public static boolean handleMaterialAcceleration(@Nonnull final World world, @Nonnull final AxisAlignedBB bb, @Nonnull final Material material, @Nonnull final Entity entity) {
            final int minX = MathHelper.floor(bb.minX), minY = MathHelper.floor(bb.minY), minZ = MathHelper.floor(bb.minZ), maxX = MathHelper.ceil(bb.maxX), maxY = MathHelper.ceil(bb.maxY), maxZ = MathHelper.ceil(bb.maxZ);
            if(!world.isAreaLoaded(minX, minY, minZ, maxX, maxY, maxZ, true)) return false;

            @Nonnull final IWaterHeight waterHeight = (IWaterHeight)entity;
            @Nonnull final BlockPos.PooledMutableBlockPos pos = BlockPos.PooledMutableBlockPos.retain();
            if(material == Material.WATER) FluidCollisionHandler.cacheHeight.set(waterHeight);

            @Nonnull final ChunkCache cache = new ChunkCache(world, new BlockPos(minX, minY, minZ), new BlockPos(maxX, maxY, maxZ), 0);
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
                if((blockFlags & 64) == 0 && ((blockFlags & 32) != 0 || !(newState.getBlock() instanceof IFluidloggable) && FluidState.removeOnBlockChange.get())) {
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
                        oldFluidState.getState().neighborChanged(world, pos, newState.getBlock(), pos);
                    }
                }
            }
        }

        public static boolean isBlockPowered(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
            @Nonnull final IBlockAccess access = world instanceof World ? new FluidCache(world, pos, 4, 4) : world;
            @Nonnull final FluidState fluidState = FluidState.get(access, pos);

            return !fluidState.isEmpty() && fluidState.getState().getWeakPower(access, pos, fluidState.getDownDensityFace()) > 0 ||
                    0 < getRedstonePower(access, pos.down(), EnumFacing.DOWN) ||
                    0 < getRedstonePower(access, pos.up(), EnumFacing.UP) ||
                    0 < getRedstonePower(access, pos.north(), EnumFacing.NORTH) ||
                    0 < getRedstonePower(access, pos.south(), EnumFacing.SOUTH) ||
                    0 < getRedstonePower(access, pos.west(), EnumFacing.WEST) ||
                    0 < getRedstonePower(access, pos.east(), EnumFacing.EAST);
        }

        public static boolean isFlammableFluidWithin(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb) {
            if(block.getDefaultState().getMaterial() == Material.LAVA) return Boolean.TRUE.equals(block.isAABBInsideLiquid(world, pos, bb));
            final FluidState fluidState = FluidState.get(world, pos); //handle possible lava FluidState
            return fluidState.getMaterial() == Material.LAVA && Boolean.TRUE.equals(fluidState.getBlock().isAABBInsideLiquid(world, pos, bb));
        }

        public static boolean isMaterialInFluidBB(@Nonnull World world, @Nonnull AxisAlignedBB bb, @Nonnull Material materialIn, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
            for(int x = minX; x < maxX; ++x) {
                for(int y = minY; y < maxY; ++y) {
                    for(int z = minZ; z < maxZ; ++z) {
                        final BlockPos pos = new BlockPos(x, y, z);
                        final FluidState fluidState = FluidState.get(world, pos);

                        if(!fluidState.isEmpty()) {
                            @Nullable Boolean result = fluidState.getBlock().isAABBInsideMaterial(world, pos, bb, materialIn);
                            if(result != null) {
                                if(!result) continue;
                                return true;
                            }
                            else if(fluidState.getMaterial() == materialIn)
                                return true;
                        }
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

        @SuppressWarnings("UnusedReturnValue")
        public static boolean setBlockToAir(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState airState, final int blockFlags) {
            @Nonnull final Chunk chunk = world.getChunk(pos);
            if(world.isRemote && FluidloggedUtils.isFluid(chunk.getBlockState(pos))) return false; // prevents possible client desync
            else return world.setBlockState(pos, FluidState.getFromProvider(chunk, pos).toFlowing().getState(), blockFlags | 32);
        }

        public static boolean useNeighborBrightness(@Nonnull World world, @Nonnull BlockPos pos) {
            return PluginChunkCache.Hooks.useNeighborBrightness(world, pos);
        }
    }

    //hold Dynamic Lights methods in separate class to avoid crash
    public static final class DLHooks
    {
        public static int getLightValue(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState fluidState) {
            return Math.max(DynamicLights.getLightValue(state.getBlock(), state, world, pos), fluidState.getLightValue(world, pos));
        }
    }
}
