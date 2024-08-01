/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * remove isEntityInsideMaterial override
 * @author jbred
 *
 */
public final class PluginLycanitesFluidBase implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.methods.removeIf(method -> method.name.equals("isEntityInsideMaterial"));
        return false;
    }
}
