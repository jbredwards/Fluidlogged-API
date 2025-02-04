/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.biomesoplenty;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * remove unnecessary event handler for BOP fluid bucket filling (and fixes a honey fluid dupe)
 * @author jbred
 *
 */
public final class PluginBucketEventHandler implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        overrideMethod(classNode, method -> method.name.equals("onRightClickHoldingBucket"), null, null, generator -> {});
        return false;
    }
}
