/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.extrautils;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * extrautils' block access wrapper FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginExtraUtilsAccessDelegate implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IBlockAccessWrapper");
        addMethod(classNode, "getWrapped", "()Lnet/minecraft/world/IBlockAccess;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "com/rwtema/extrautils2/utils/blockaccess/BlockAccessDelegate", "base", "Lnet/minecraft/world/IBlockAccess;");
        });

        return false;
    }
}
