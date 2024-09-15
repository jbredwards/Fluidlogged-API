/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.openblocks;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * make openblocks' sponge FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginOpenBlocks implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(method.name.equals("updateNeigbouringLiquids")) return 1;
        else return method.name.equals("clearupLiquid") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * updateNeigbouringLiquids:
         * Old code:
         * IBlockState state = world.getBlockState(workPos);
         *
         * New code:
         * // account for FluidStates
         * IBlockState state = FluidloggedUtils.getFluidOrReal(world, workPos);
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * clearupLiquid:
         * Old code:
         * IBlockState state = world.getBlockState(workPos);
         * ...
         * world.setBlockState(pos.offset(dx, dy, dz), Blocks.AIR.getDefaultState(), cleanupFlags);
         *
         * New code:
         * // account for FluidStates
         * IBlockState state = FluidloggedUtils.getFluidOrReal(world, workPos);
         * ...
         * PluginBlockSponge.Hooks.drain(world, pos.offset(dx, dy, dz), cleanupFlags, state);
         */
        else if(index == 2) {
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
            }
            else if(checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
                removeFrom(instructions, getPrevious(insn, 2), -1);
                instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/block/PluginBlockSponge$Hooks", "drain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ILnet/minecraft/block/state/IBlockState;)Z"));
                instructions.insert(insn, new VarInsnNode(ALOAD, 9));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }
}
