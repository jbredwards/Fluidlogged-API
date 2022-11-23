package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

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
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/asm/impl/IChunkProvider");
        /*
         * New code:
         * //allows optifine's ChunkCacheOF override to be compatible with this mod
         * @ASMGenerated
         * public Chunk getChunkFromBlockCoords(BlockPos pos)
         * {
         *     return this.chunkCache.getChunkFromBlockCoords(pos);
         * }
         */
        addMethod(classNode, "getChunkFromBlockCoords", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/optifine/override/ChunkCacheOF", "chunkCache", "Lnet/minecraft/world/ChunkCache;");
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/ChunkCache", "getChunkFromBlockCoords", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", false);
        });

        return false;
    }
}
