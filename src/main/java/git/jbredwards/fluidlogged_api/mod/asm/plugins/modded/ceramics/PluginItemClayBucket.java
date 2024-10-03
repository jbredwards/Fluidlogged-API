/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.ceramics;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fluids.FluidStack;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * offset the placement pos if the block is fluidloggable, so it can be fluidlogged
 * @author jbred
 *
 */
public final class PluginItemClayBucket implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return "onBucketEvent".equals(method.name); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * onBucketEvent:
         * Old code:
         * result = this.tryPlaceFluid(stack, player, world, targetPos);
         *
         * New code:
         * // offset the placement pos if the block is fluidloggable, so it can be fluidlogged
         *  result = tryPlaceFluid(stack, player, world, Hooks.getPos(targetPos, pos, player, state, this.getFluid(stack)));
         */
        if(checkMethod(insn, "tryPlaceFluid")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 5));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 6));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 7));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "knightminer/ceramics/items/ItemClayBucket", "getFluid", "(Lnet/minecraft/item/ItemStack;)Lnet/minecraftforge/fluids/FluidStack;", false));
            instructions.insertBefore(insn, genMethodNode("getPos", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/block/state/IBlockState;Lnet/minecraftforge/fluids/FluidStack;)Lnet/minecraft/util/math/BlockPos;"));
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static BlockPos getPos(@Nonnull final BlockPos prevTarget, @Nonnull final BlockPos tracePos, @Nonnull final EntityPlayer player, @Nonnull final IBlockState traceState, @Nullable final FluidStack fluid) {
            return fluid != null && FluidloggedAPIConfig.bucketFluidlogging.test(player) && FluidloggedUtils.isStateFluidloggable(traceState, player.world, tracePos, FluidState.of(fluid.getFluid())) ? tracePos : prevTarget;
        }
    }
}
