package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fix lighting bugs
 * @author jbred
 *
 */
public final class PluginChunkCache implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_175629_a" : "getLightForExt"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //moved from cubic chunks for compat
        if(insn.getOpcode() == IF_ICMPGE) {
            ((JumpInsnNode)insn).setOpcode(IFNE);
            removeFrom(instructions, insn.getPrevious(), -5);

            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/ChunkCache", obfuscated ? "field_72815_e" : "world", "Lnet/minecraft/world/World;"));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_189509_E" : "isOutsideBuildHeight", "(Lnet/minecraft/util/math/BlockPos;)Z", false));
            instructions.insertBefore(insn, list);
        }
        //use FluidState if possible
        else if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IChunkProvider");
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
    }
}
