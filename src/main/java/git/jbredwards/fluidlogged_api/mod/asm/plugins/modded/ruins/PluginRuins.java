/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.ruins;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraftforge.common.util.Constants;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * Forcibly prevent Ruins structures from being able to contain old FluidStates.
 * @author jbred
 *
 */
public final class PluginRuins implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("levelSite") || method.name.equals("realizeBlock"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * world.setBlockState(position, state, 2);
         *
         * New code:
         * // Forcibly prevent Ruins structures from being able to contain old FluidStates.
         * world.setBlockState(position, state, 2 | 48);
         */
        if(checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
            instructions.insertBefore(insn, new IntInsnNode(BIPUSH, 32 | Constants.BlockFlags.NO_OBSERVERS));
            instructions.insertBefore(insn, new InsnNode(IOR));
        }

        return false;
    }
}
