/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.astral_sorcery;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * make astral sorcery's block access wrappers FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginAstralSorceryAccess implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        // make these clientside only classes actually clientside, so the game crashes if other mods use them serverside
        classNode.visitAnnotation("Lnet/minecraftforge/fml/relauncher/SideOnly;", true).visitEnum("value", "Lnet/minecraftforge/fml/relauncher/Side;", "CLIENT");
        // astral sorcery structures don't use FluidStates, so all of these are empty
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IWorldProvider");
        addMethod(classNode, "getChunk", "(II)Lnet/minecraft/world/chunk/Chunk;", null, null, generator ->
            generator.visitInsn(ACONST_NULL));
        addMethod(classNode, "getFluidState", "(III)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null, generator ->
            generator.visitFieldInsn(GETSTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "EMPTY", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
        addMethod(classNode, "getWorld", "()Lnet/minecraft/world/World;", null, null, generator -> {
            generator.visitMethodInsn(INVOKESTATIC, "net/minecraftforge/fml/client/FMLClientHandler", "instance", "()Lnet/minecraftforge/fml/client/FMLClientHandler;", false);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fml/client/FMLClientHandler", "getWorldClient", "()Lnet/minecraft/client/multiplayer/WorldClient;", false);
        });
        return false;
    }
}
