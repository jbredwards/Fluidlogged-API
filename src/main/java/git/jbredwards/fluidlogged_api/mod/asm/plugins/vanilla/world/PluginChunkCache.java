package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.util.math.BlockPos;
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

        return false;
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
    }
}
