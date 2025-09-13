/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_expansion;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * Make Thermal Expansion's machines FluidState-sensitive.
 * @author jbred
 *
 */
public final class PluginThermalExpansion implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("updateValidity"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * IBlockState state = this.world.getBlockState(this.pos);
         *
         * New code:
         * // Account for FluidStates.
         * IBlockState state = FluidloggedUtils.getFluidOrReal(this.world, this.pos);
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
        }

        return false;
    }
}
