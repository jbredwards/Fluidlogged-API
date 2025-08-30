/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
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
    private int toRemove;

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
        if(checkField(insn, "gratingWater")) removeFrom(instructions, insn, toRemove);
        else if(checkField(insn, "gratingLava")) {
            removeFrom(instructions, insn, toRemove);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        toRemove = classNode.methods.stream().anyMatch(method -> checkMethod(method, "registerBlock", "(Lnet/minecraft/block/Block;Ljava/lang/Class;)V")) ? 2 : 4;
        return true;
    }
}
