/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tropicraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * fixes for tropicraft fences
 * @author jbred
 *
 */
public final class PluginTropicraftFence implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        // remove methods that deal with the (now unused) "WATER" blockState property
        classNode.methods.removeIf(method
                -> method.name.equals(obfuscated ? "func_149688_o" : "getMaterial")
                || method.name.equals(obfuscated ? "func_176201_c" : "getMetaFromState")
                || method.name.equals(obfuscated ? "func_176203_a" : "getStateFromMeta")
                || method.name.equals(obfuscated ? "func_189540_a" : "neighborChanged")
                || method.name.equals(obfuscated ? "func_176213_c" : "onBlockAdded")
                || method.name.equals("canRenderInLayer")
                || method.name.equals("getStateForPlacement"));

        // make tropicraft fences override the "applyDefaults" setting, as they must be fluidloggable for world gen
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        addMethod(classNode, "overrideApplyDefaultsSetting", "(Lnet/minecraft/block/state/IBlockState;)Z", null, null, generator -> generator.visitInsn(ICONST_1));
        return false;
    }
}
