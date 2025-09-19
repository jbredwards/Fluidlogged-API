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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.world.IChunkProvider;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * implements IChunkProvider
 * @author jbred
 *
 */
public final class PluginChunkCache implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_175629_a" : "getLightForExt"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getLightForExt: (changes are around line 134)
         * Old code:
         * if (this.getBlockState(pos).useNeighborBrightness())
         * {
         *     ...
         * }
         *
         * New code:
         * //fix neighbor brightness related bugs
         * if (Hooks.useNeighborBrightness(this, pos))
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, obfuscated ? "func_185916_f" : "useNeighborBrightness")) {
            instructions.insert(insn, genMethodNode("useNeighborBrightness", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
            removeFrom(instructions, insn, -1);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * New code:
         * // allows this to provide its world instance
         * @ASMGenerated
         * public World getWorld()
         * {
         *     return this.world;
         * }
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IWorldProvider");
        addMethod(classNode, "getWorld", "()Lnet/minecraft/world/World;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/world/ChunkCache", obfuscated ? "field_72815_e" : "world", "Lnet/minecraft/world/World;");
        });
        /*
         * New code:
         * // allows this to provide its chunks, which allows this mod to access its FluidStates
         * @ASMGenerated
         * public Chunk getChunk(int chunkX, int chunkZ)
         * {
         *     return Hooks.getChunkFromChunkCache(chunkX, chunkZ, this.chunkArray, this.chunkX, this.chunkZ);
         * }
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IChunkProvider");
        addMethod(classNode, "getChunk", "(II)Lnet/minecraft/world/chunk/Chunk;",
            "getChunkFromChunkCache", "(II[[Lnet/minecraft/world/chunk/Chunk;II)Lnet/minecraft/world/chunk/Chunk;", generator -> {
                generator.visitVarInsn(ILOAD, 1);
                generator.visitVarInsn(ILOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/world/ChunkCache", obfuscated ? "field_72817_c" : "chunkArray", "[[Lnet/minecraft/world/chunk/Chunk;");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/world/ChunkCache", obfuscated ? "field_72818_a" : "chunkX", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/world/ChunkCache", obfuscated ? "field_72816_b" : "chunkZ", "I");
            }
        );

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static Chunk getChunkFromChunkCache(int chunkXIn, int chunkZIn, @Nonnull Chunk[][] chunkArray, int chunkX, int chunkZ) {
            final int x = chunkXIn - chunkX;
            final int z = chunkZIn - chunkZ;
            return x >= 0 && x < chunkArray.length && z >= 0 && z < chunkArray[x].length ? chunkArray[x][z] : null;
        }

        public static boolean useNeighborBrightness(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            @Nullable final Chunk chunk = ((IChunkProvider)world).getChunk(pos);
            return chunk != null && useNeighborBrightness(chunk.getBlockState(pos), world, pos, chunk);
        }

        //helper
        public static boolean useNeighborBrightness(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return state.useNeighborBrightness() || FluidState.getFromProvider(chunk, pos).getState().useNeighborBrightness();
        }
    }
}
