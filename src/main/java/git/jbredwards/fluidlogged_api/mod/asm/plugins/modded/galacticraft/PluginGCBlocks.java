/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * don't register Galacticraft's water & lava grating blocks, so they can be remapped
 * @author jbred
 *
 */
public final class PluginGCBlocks implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("registerBlocks"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * registerBlocks:
         * Old code:
         * registerBlock(gratingWater, (Class)null);
         * registerBlock(gratingLava, (Class)null);
         *
         * New code:
         * // don't register Galacticraft's water & lava grating blocks, so they can be remapped
         * ...
         * ...
         */
        if(checkField(insn, "gratingWater")) removeFrom(instructions, insn, 2);
        else if(checkField(insn, "gratingLava")) {
            removeFrom(instructions, insn, 2);
            return true;
        }

        return false;
    }
}
