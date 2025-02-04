/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.enderio;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * make ender io's sponge (industrial insulation block) FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginEnderIOSponge implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("absorb"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * absorb:
         * Old code:
         * IBlockState state = worldIn.getBlockState(blockpos1);
         *
         * New code:
         * // account for FluidStates
         * IBlockState state = FluidloggedUtils.getFluidOrReal(worldIn, blockpos1);
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
        }
        /*
         * absorb:
         * Old code:
         * worldIn.setBlockState(pos.offset(dx, dy, dz), Blocks.AIR.getDefaultState(), 2);
         *
         * New code:
         * // account for FluidStates
         * PluginBlockSponge.Hooks.drain(worldIn, pos.offset(dx, dy, dz), 2, state);
         */
        else if(checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
            removeFrom(instructions, getPrevious(insn, 2), -1);
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/block/PluginBlockSponge$Hooks", "drain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ILnet/minecraft/block/state/IBlockState;)Z"));
            instructions.insert(insn, new VarInsnNode(ALOAD, findLocal(method, "blockToCheck", "Lnet/minecraft/block/state/IBlockState;").index));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
