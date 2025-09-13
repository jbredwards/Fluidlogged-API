/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.pneumaticcraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * Allow Empty PCBs to be filled using FluidStates.
 * @author jbred
 *
 */
public final class PluginItemEmptyPCB implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("onEntityItemUpdate"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * if (Fluids.areFluidsEqual(FluidRegistry.lookupFluidForBlock(entityItem.world.getBlockState(new BlockPos(entityItem)).getBlock()), Fluids.ETCHING_ACID))
         * {
         *     ...
         * }
         *
         * New code:
         * // Allow Empty PCBs to be filled using FluidStates.
         * if (Fluids.areFluidsEqual(FluidRegistry.lookupFluidForBlock(FluidloggedUtils.getFluidOrReal(entityItem.world, new BlockPos(entityItem)).getBlock()), Fluids.ETCHING_ACID))
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
