/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tropicraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix issue#183
 * @author jbred
 *
 */
public final class PluginTropicraftFluid implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/fluid/ICompatibleFluid");
        addMethod(classNode, "getParentFluid", "()Lnet/minecraftforge/fluids/Fluid;", null, null, generator -> generator.visitFieldInsn(GETSTATIC, "net/minecraftforge/fluids/FluidRegistry", "WATER", "Lnet/minecraftforge/fluids/Fluid;"));
        return false;
    }
}
