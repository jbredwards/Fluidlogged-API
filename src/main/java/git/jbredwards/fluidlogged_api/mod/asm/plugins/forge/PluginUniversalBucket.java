/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.FluidStack;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * only offset the fluid placement pos if the block isn't replaceable or fluidloggable
 * @author jbred
 *
 */
public final class PluginUniversalBucket implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_77659_a" : "onItemRightClick"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * onItemRightClick: (changes are around line 183)
         * Old code:
         * BlockPos targetPos = clickPos.offset(mop.sideHit);
         *
         * New code:
         * // The block adjacent to the side we clicked on if the block there isn't replaceable or fluidloggable
         * BlockPos targetPos = Hooks.getTargetPos(clickPos, mop.sideHit, world, player, fluidStack);
         */
        if(checkMethod(insn, obfuscated ? "func_177972_a" : "offset")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 5));
            instructions.insertBefore(insn, genMethodNode("getTargetPos", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraft/world/World;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraftforge/fluids/FluidStack;)Lnet/minecraft/util/math/BlockPos;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static BlockPos getTargetPos(@Nonnull final BlockPos pos, @Nonnull final EnumFacing side, @Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final FluidStack fluidStack) {
            @Nonnull final IBlockState state = world.getBlockState(pos);
            return side == EnumFacing.UP && state.getBlock().isReplaceable(world, pos) || FluidloggedAPIConfig.bucketFluidlogging.test(player)
                    && FluidloggedUtils.isStateFluidloggable(state, world, pos, FluidState.of(fluidStack.getFluid())) ? pos : pos.offset(side);
        }
    }
}
