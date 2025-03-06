/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.chiseled_me;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * fix chiseled me conflict
 * @author jbred
 *
 */
public final class PluginChiseledMe implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        //fluidlogged api already implements these patches
        classNode.methods.removeIf(method -> method.name.equals("rayTraceBlocks") || method.name.equals("isInLava"));
        return true;
    }
}
