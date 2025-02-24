/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.BlockSnow;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * Fix grass & mycelium growing and not decaying underwater
 * @author jbred
 *
 */
public final class PluginBlockGrass implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_180650_b" : "updateTick"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * updateTick: (changes are around line 46)
         * Old code:
         * if (worldIn.getLightFromNeighbors(pos.up()) < 4 && worldIn.getBlockState(pos.up()).getLightOpacity(worldIn, pos.up()) > 2)
         * {
         *     ...
         * }
         *
         * New code:
         * if (cannotSurviveAt(worldIn, pos.up()))
         * {
         *     ...
         * }
         */
        if(insn.getOpcode() == ICONST_2 && insn.getNext().getOpcode() == IF_ICMPLE) {
            ((JumpInsnNode)insn.getNext()).setOpcode(IFEQ);
            instructions.insert(insn, genMethodNode("cannotSurviveAt", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
            removeFrom(instructions, insn, -11);
        }
        /*
         * updateTick: (changes are around line 46)
         * Old code:
         * if (iblockstate1.getBlock() == Blocks.DIRT && iblockstate1.getValue(BlockDirt.VARIANT) == BlockDirt.DirtType.DIRT && worldIn.getLightFromNeighbors(blockpos.up()) >= 4 && iblockstate.getLightOpacity(worldIn, pos.up()) <= 2)
         * {
         *     ...
         * }
         *
         * New code:
         * if (iblockstate1.getBlock() == Blocks.DIRT && iblockstate1.getValue(BlockDirt.VARIANT) == BlockDirt.DirtType.DIRT && canSpreadTo(worldIn, blockpos.up()))
         * {
         *     ...
         * }
         */
        else if(insn.getOpcode() == ICONST_2 && insn.getNext().getOpcode() == IF_ICMPGT) {
            ((JumpInsnNode)insn.getNext()).setOpcode(IFEQ);
            instructions.insert(insn, genMethodNode("canSpreadTo", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
            removeFrom(instructions, insn, -8);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean cannotSurviveAt(@Nonnull final World world, @Nonnull final BlockPos pos) {
            @Nonnull final IBlockState state = world.getBlockState(pos);
            if(state.getBlock() instanceof BlockSnow && state.getValue(BlockSnow.LAYERS) == 1) return false;
            else return !FluidloggedUtils.getFluidState(world, pos, state).isEmpty() || state.getLightOpacity(world, pos) > 2 && world.getLightFromNeighbors(pos) < 4;
        }

        public static boolean canSpreadTo(@Nonnull final World world, @Nonnull final BlockPos pos) {
            @Nonnull final IBlockState state = world.getBlockState(pos);
            if(state.getBlock() instanceof BlockSnow && state.getValue(BlockSnow.LAYERS) == 1) return true;
            else return FluidloggedUtils.getFluidState(world, pos, state).isEmpty() && state.getLightOpacity(world, pos) <= 2 && world.getLightFromNeighbors(pos) >= 4;
        }
    }
}
