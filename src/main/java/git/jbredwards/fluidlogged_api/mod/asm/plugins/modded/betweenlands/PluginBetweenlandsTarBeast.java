/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * tar beast cannot be pushed by fluids
 * @author jbred
 *
 */
public final class PluginBetweenlandsTarBeast implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        addMethod(classNode, obfuscated ? "func_96092_aw" : "isPushedByWater", "()Z", null, null, generator -> generator.visitInsn(ICONST_0));
        return false;
    }
}
