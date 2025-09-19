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
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import net.minecraft.block.material.Material;
import net.minecraft.block.properties.PropertyInteger;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fire doesn't destroy fluidlogged fluids
 * @author jbred
 *
 */
public final class PluginBlockFire implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("tryCatchFire"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * tryCatchFire: (changes are around line 297)
         * Old code:
         * int i = worldIn.getBlockState(pos).getBlock().getFlammability(worldIn, pos, face);
         *
         * New code:
         * //account for FluidState flammability
         * int i = Hooks.getFlammability(worldIn, pos, face);
         */
        if(checkMethod(insn, "getFlammability")) {
            instructions.insert(insn, genMethodNode("getFlammability", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 6));
            removeFrom(instructions, insn, -5);
        }
        /*
         * tryCatchFire: (changes are around line 312)
         * Old code:
         * worldIn.setBlockState(pos, this.getDefaultState().withProperty(AGE, Integer.valueOf(j)), 3);
         *
         * New code:
         * //fire doesn't destroy fluidlogged fluids
         * worldIn.setBlockState(pos, Hooks.getFireOrFluid(this.getDefaultState(), AGE, Integer.valueOf(j), worldIn, pos), 3);
         */
        else if(checkMethod(insn, obfuscated ? "func_177226_a" : "withProperty")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(genMethodNode("getFireOrFluid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/properties/PropertyInteger;Ljava/lang/Integer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176538_m" : "getNeighborEncouragement"),
            "getNeighborEncouragement", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        overrideMethod(classNode, method -> checkMethod(method, "canCatchFire", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"),
            "canCatchFire", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canCatchFire(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final EnumFacing side) {
            @Nonnull final FluidCache cache = new FluidCache(access, pos, 0, 0);
            @Nonnull final IBlockState state = cache.getBlockState(pos);

            final boolean blockFlammable = state.getBlock().isFlammable(cache, pos, side);
            if(!blockFlammable) return false;
            else if(FluidloggedUtils.isFluid(state) || !FluidloggedAPIConfig.fluidStateIsFireInsulator) return true;

            @Nonnull final FluidState fluidState = cache.getFluidState(pos);
            return fluidState == FluidState.EMPTY || fluidState.getMaterial() == Material.LAVA || fluidState.getBlock().isFlammable(cache, pos, side);
        }

        @Nonnull
        public static IBlockState getFireOrFluid(@Nonnull IBlockState fire, @Nonnull PropertyInteger ageProp, @Nonnull Integer newAge, @Nonnull World world, @Nonnull BlockPos pos) {
            final FluidState fluidState = FluidState.get(world, pos);
            return fluidState == FluidState.EMPTY || fluidState.getMaterial().getCanBurn() ? fire.withProperty(ageProp, newAge) : fluidState.getState();
        }

        public static int getFlammability(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing side) {
            @Nonnull final Chunk chunk = world.getChunk(pos);

            @Nonnull final IBlockState state = chunk.getBlockState(pos);
            final int blockFlammability = state.getBlock().getFlammability(world, pos, side);
            if(blockFlammability == 0) return 0;

            if(FluidloggedUtils.isFluid(state)) return blockFlammability;
            @Nonnull final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
            if(fluidState == FluidState.EMPTY || fluidState.getMaterial() == Material.LAVA) return blockFlammability;

            final int fluidFlammability = fluidState.getBlock().getFlammability(world, pos, side);
            if(fluidFlammability == 0) return FluidloggedAPIConfig.fluidStateIsFireInsulator ? 0 : blockFlammability;
            else return blockFlammability + fluidFlammability;
        }

        public static int getNeighborEncouragement(@Nonnull final World world, @Nonnull final BlockPos pos) {
            @Nonnull final FluidCache cache = new FluidCache(world, pos, 1, 1);
            if(!cache.isAirBlock(pos)) return 0;

            int maxEncouragement = 0;
            for(@Nonnull final EnumFacing side : EnumFacing.VALUES) {
                @Nonnull final BlockPos offset = pos.offset(side);
                @Nonnull final IBlockState state = cache.getBlockState(pos);

                final int blockEncouragement = state.getBlock().getFireSpreadSpeed(cache, offset, side.getOpposite());
                if(blockEncouragement == 0) continue;

                else if(FluidloggedUtils.isFluid(state)) {
                    maxEncouragement = Math.max(blockEncouragement, maxEncouragement);
                    continue;
                }

                @Nonnull final FluidState fluidState = cache.getFluidState(offset);
                if(fluidState == FluidState.EMPTY || fluidState.getMaterial() == Material.LAVA) {
                    maxEncouragement = Math.max(blockEncouragement, maxEncouragement);
                    continue;
                }

                final int fluidEncouragement = fluidState.getBlock().getFireSpreadSpeed(cache, offset, side.getOpposite());
                if(fluidEncouragement == 0) {
                    if(!FluidloggedAPIConfig.fluidStateIsFireInsulator) maxEncouragement = Math.max(blockEncouragement, maxEncouragement);
                }

                else maxEncouragement = Math.max(blockEncouragement + fluidEncouragement, maxEncouragement);
            }

            return maxEncouragement;
        }
    }
}
