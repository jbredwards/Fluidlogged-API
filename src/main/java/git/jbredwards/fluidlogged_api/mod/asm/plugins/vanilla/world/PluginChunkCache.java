package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.asm.impl.IChunkProvider;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
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
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/asm/impl/IChunkProvider");
        addMethod(classNode, "getChunkFromBlockCoords", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;",
            "getChunkFromChunkCache", "(Lnet/minecraft/util/math/BlockPos;[[Lnet/minecraft/world/chunk/Chunk;II)Lnet/minecraft/world/chunk/Chunk;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
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
        public static Chunk getChunkFromChunkCache(@Nonnull BlockPos pos, @Nonnull Chunk[][] chunkArray, int chunkX, int chunkZ) {
            final int x = (pos.getX() >> 4) - chunkX;
            final int z = (pos.getZ() >> 4) - chunkZ;
            return x >= 0 && x < chunkArray.length && z >= 0 && z < chunkArray[x].length ? chunkArray[x][z] : null;
        }

        public static boolean useNeighborBrightness(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            final @Nullable Chunk chunk = IChunkProvider.getChunk(world, pos);
            if(chunk == null) return false;

            final IBlockState state = chunk.getBlockState(pos);
            if(state.useNeighborBrightness()) {
                final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
                if(fluidState.isEmpty() || fluidState.getState().useNeighborBrightness()) return true;
                return state.getLightOpacity(world, pos) > fluidState.getState().getLightOpacity(world, pos);
            }

            return false;
        }
    }
}
