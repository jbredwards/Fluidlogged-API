package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * allows the game to render FluidStates
 * @author jbred
 *
 */
public final class PluginRenderChunk implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, obfuscated ? "func_178581_b" : "rebuildChunk", null);
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        final boolean isBetterFoliage = checkMethod(insn, "canRenderBlockInLayer", null);

        //VANILLA
        if(checkMethod(insn, "canRenderInLayer", null) || (isBetterFoliage && ((VarInsnNode)getPrevious(insn, 2)).var == 15)) {
            final InsnList list = new InsnList();
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 11));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //world variable
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/client/renderer/chunk/RenderChunk", (obfuscated ? "field_189564_r" : "worldView"), "Lnet/minecraft/world/ChunkCache;"));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 14));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //adds the new code
            list.add(genMethodNode("renderChunk", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/BlockRenderLayer;[ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);

            setMaxLocals(method, 9);
            return true;
        }

        //OPTIFINE (with Better Foliage)
        else if(isBetterFoliage && ((VarInsnNode)getPrevious(insn, 2)).var == 18) {
            final InsnList list = new InsnList();
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 12));
            //render chunk variable
            list.add(new VarInsnNode(ALOAD, 0));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //chunkCacheOF variable
            list.add(new VarInsnNode(ALOAD, 11));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 17));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //adds the new code
            list.add(genMethodNode("renderChunkOF", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/BlockRenderLayer;[ZLnet/minecraft/client/renderer/chunk/RenderChunk;Lnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);

            setMaxLocals(method, 10);
            return true;
        }

        //OPTIFINE (without Better Foliage)
        else if(checkMethod(insn, "callBoolean", null)) {
            final InsnList list = new InsnList();
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 12));
            //render chunk variable
            list.add(new VarInsnNode(ALOAD, 0));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //chunkCacheOF variable
            list.add(new VarInsnNode(ALOAD, 11));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 17));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //adds the new code
            list.add(genMethodNode("renderChunkOF", "(Ljava/lang/Object;Ljava/lang/Object;[Ljava/lang/Object;[ZLnet/minecraft/client/renderer/chunk/RenderChunk;Lnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);

            setMaxLocals(method, 10);
            return true;
        }

        return false;
    }
}
