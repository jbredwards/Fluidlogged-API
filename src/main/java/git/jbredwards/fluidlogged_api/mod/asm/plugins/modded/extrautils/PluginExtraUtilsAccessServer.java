/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
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
public final class PluginExtraUtilsAccessServer implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IBlockAccessWrapper");
        addMethod(classNode, "getWrapped", "()Lnet/minecraft/world/IBlockAccess;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "com/rwtema/extrautils2/utils/blockaccess/ThreadSafeBlockAccess", "world", "Lnet/minecraft/world/WorldServer;");
        });
        /*
         * public Chunk getChunk(int chunkX, int chunkZ)
         * {
         *     return (Chunk)this.chunkMap.get(ChunkPos.asLong(chunkX, chunkZ));
         * }
         */
        addMethod(classNode, "getChunk", "(II)Lnet/minecraft/world/chunk/Chunk;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "com/rwtema/extrautils2/utils/blockaccess/ThreadSafeBlockAccess", "chunkMap", "Lit/unimi/dsi/fastutil/longs/Long2ObjectMap;");
            generator.visitVarInsn(ILOAD, 1);
            generator.visitVarInsn(ILOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, "net/minecraft/util/math/ChunkPos", obfuscated ? "func_77272_a" : "asLong", "(II)J", false);
            generator.visitMethodInsn(INVOKEINTERFACE, "it/unimi/dsi/fastutil/longs/Long2ObjectMap", "get", "(J)Ljava/lang/Object;", true);
            generator.visitTypeInsn(CHECKCAST, "net/minecraft/world/chunk/Chunk");
        });

        return false;
    }
}
