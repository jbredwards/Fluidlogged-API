package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.asm.impl.IChunkProvider;
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
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/asm/impl/IChunkProvider");
        /*
         * New code:
         * //allows nothirium's ChunkCache override to be compatible with this mod
         * @ASMGenerated
         * public Chunk getChunkFromBlockCoords(BlockPos pos)
         * {
         *     return this.getChunk(pos);
         * }
         */
        addMethod(classNode, "getChunkFromBlockCoords", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, classNode.name, "getChunk", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", false);
        });

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean useNeighborBrightness(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            final @Nullable Chunk chunk = IChunkProvider.getChunk(world, pos);
            return chunk != null && PluginChunkCache.Hooks.useNeighborBrightness(state, world, pos, chunk);
        }
    }
}
