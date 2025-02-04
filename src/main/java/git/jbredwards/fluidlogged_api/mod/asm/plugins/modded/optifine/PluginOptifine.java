/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.optifine;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * better optifine compat
 * @author jbred
 *
 */
public final class PluginOptifine implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IWorldProvider");
        addMethod(classNode, "getWorld", "()Lnet/minecraft/world/World;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/optifine/override/ChunkCacheOF", "chunkCache", "Lnet/minecraft/world/ChunkCache;");
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/ChunkCache", "getWorld", "()Lnet/minecraft/world/World;", false);
        });
        /*
         * New code:
         * //allows optifine's ChunkCacheOF override to be compatible with this mod
         * @ASMGenerated
         * public Chunk getChunk(int chunkX, int chunkZ)
         * {
         *     return this.chunkCache.getChunk(chunkX, chunkZ);
         * }
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IChunkProvider");
        addMethod(classNode, "getChunk", "(II)Lnet/minecraft/world/chunk/Chunk;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/optifine/override/ChunkCacheOF", "chunkCache", "Lnet/minecraft/world/ChunkCache;");
            generator.visitVarInsn(ILOAD, 1);
            generator.visitVarInsn(ILOAD, 2);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/ChunkCache", "getChunk", "(II)Lnet/minecraft/world/chunk/Chunk;", false);
        });

        return false;
    }
}
