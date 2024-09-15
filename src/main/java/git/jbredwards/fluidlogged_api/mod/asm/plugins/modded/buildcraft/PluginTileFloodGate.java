/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.buildcraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * make buildcraft's TileFloodGate FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginTileFloodGate implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("canFill"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * canFill:
         * Old code:
         * this.getLocalState(offsetPos)
         *
         * New code:
         * // account for FluidStates
         * Hooks.getFluidOrReal(this, offsetPos)
         */
        if(checkMethod(insn, "getLocalState")) {
            instructions.insert(insn, genMethodNode("getFluidOrReal", "(Lnet/minecraft/tileentity/TileEntity;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            removeFrom(instructions, insn, -1);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static IBlockState getFluidOrReal(@Nonnull final TileEntity tile, @Nonnull final BlockPos pos) {
            return FluidloggedUtils.getFluidOrReal(tile.getWorld(), pos);
        }
    }
}
