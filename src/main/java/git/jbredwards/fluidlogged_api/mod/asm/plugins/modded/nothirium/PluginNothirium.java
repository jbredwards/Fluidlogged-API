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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.nothirium;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.world.IChunkProvider;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.PluginChunkCache;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * better nothirium compat
 * @author jbred
 *
 */
public final class PluginNothirium implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_175626_b" : "getCombinedLight"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getCombinedLight:
         * Old code:
         * if (state.useNeighborBrightness())
         * {
         *     ...
         * }
         *
         * New code:
         * //fix neighbor brightness related bugs
         * if (Hooks.useNeighborBrightness(state, this, pos))
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, obfuscated ? "func_185916_f" : "useNeighborBrightness")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, genMethodNode("useNeighborBrightness", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IWorldProvider");
        addMethod(classNode, "getWorld", "()Lnet/minecraft/world/World;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "meldexun/nothirium/mc/renderer/chunk/SectionRenderCache", "world", "Lnet/minecraft/world/World;");
        });
        /*
         * New code:
         * //allows nothirium's ChunkCache override to be compatible with this mod
         * @ASMGenerated
         * public Chunk getChunk(int chunkX, int chunkZ)
         * {
         *     return (Chunk)this.chunkCache.get(chunkX, chunkZ);
         * }
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IChunkProvider");
        addMethod(classNode, "getChunk", "(II)Lnet/minecraft/world/chunk/Chunk;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "meldexun/nothirium/mc/renderer/chunk/SectionRenderCache", "chunkCache", "Lmeldexun/nothirium/util/cache/Cache2D;");
            generator.visitVarInsn(ILOAD, 1);
            generator.visitVarInsn(ILOAD, 2);
            generator.visitMethodInsn(INVOKEVIRTUAL, "meldexun/nothirium/util/cache/Cache2D", "get", "(II)Ljava/lang/Object;", false);
            generator.visitTypeInsn(CHECKCAST, "net/minecraft/world/chunk/Chunk");
        });

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean useNeighborBrightness(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            final @Nullable Chunk chunk = ((IChunkProvider)world).getChunk(pos);
            return chunk != null && PluginChunkCache.Hooks.useNeighborBrightness(state, world, pos, chunk);
        }
    }
}
