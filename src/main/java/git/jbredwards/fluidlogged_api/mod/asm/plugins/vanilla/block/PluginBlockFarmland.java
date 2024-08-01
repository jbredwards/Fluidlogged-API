/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * farmland blocks now recognise water FluidStates
 * @author jbred
 *
 */
public final class PluginBlockFarmland implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        switch(method.name) {
            case "func_176530_e": case "hasWater": case "hasLava": // Nethercraft compat
                return true;
            default: return false;
        }
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * hasWater: (changes are around line 119)
         * Old code:
         * if (worldIn.getBlockState(blockpos$mutableblockpos).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         *
         * New code:
         * //check for fluidlogged blocks
         * if (FluidloggedUtils.getFluidOrReal(worldIn, blockpos$mutableblockpos).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         */
        //separate obfuscated check to resolve foamfix conflict
        if(obfuscated && checkMethod(insn, "func_180495_p") || checkMethod(insn, "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
        }

        return false;
    }
}
