package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public final class RenderChunkPlugin extends AbstractPlugin
{
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_178581_b" : "rebuildChunk";
    }

    @Nullable
    @Override
    public String getMethodDesc() {
        return "(FFFLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;)V";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //OPTIFINE, line 299 (switches to fluid render for fluidlogged blocks)
        if(ASMUtils.checkMethod(insn, "callBoolean", null)) {
            final InsnList list = new InsnList();
            //IBlockAccess local var
            list.add(new VarInsnNode(ALOAD, 11));
            //BlockPos local var
            list.add(new VarInsnNode(ALOAD, 17));
            //adds the new code
            list.add(method("canRenderInLayerOF", "(Ljava/lang/Object;Ljava/lang/Object;[Ljava/lang/Object;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return false;
        }
        //VANILLA, line 189 (switches to fluid render for fluidlogged blocks)
        if(ASMUtils.checkMethod(insn, "canRenderInLayer", null)) {
            final InsnList list = new InsnList();
            //IBlockAccess var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/client/renderer/chunk/RenderChunk", (obfuscated ? "field_189564_r" : "worldView"), "Lnet/minecraft/world/ChunkCache;"));
            //BlockPos local var
            list.add(new VarInsnNode(ALOAD, 14));
            //adds the new code
            list.add(method("canRenderInLayer", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/BlockRenderLayer;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return false;
        }
        //BOTH, line 203 (renders the fluid block rather than the actual block for fluidlogged blocks)
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_175018_a" : "renderBlock", null)) {
            instructions.insert(insn, method("renderBlock", "(Lnet/minecraft/client/renderer/BlockRendererDispatcher;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/client/renderer/BufferBuilder;)Z"));
            instructions.remove(insn);
            return false;
        }
        //OPTIFINE (corrects the fluidlogged fluid block OF shader)
        if(ASMUtils.checkMethod(insn, "getRenderEnv", null)) {
            instructions.insert(ASMUtils.getPrevious(insn, 2), method("fixFluidState", "(Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            return false;
        }
        //OPTIFINE (adds stored block render)
        if(ASMUtils.checkMethod(insn, "callVoid", null) && ASMUtils.getPrevious(insn, 2).getOpcode() == ACONST_NULL) {
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 12));
            //render chunk variable
            list.add(new VarInsnNode(ALOAD, 0));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //iblockstate variable
            list.add(new VarInsnNode(ALOAD, 18));
            //chunkCacheOF variable
            list.add(new VarInsnNode(ALOAD, 11));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 17));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "renderFluidloggedBlockOF", "([ZLnet/minecraft/client/renderer/chunk/RenderChunk;Lnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V", false));

            instructions.insert(insn, list);
            return true;
        }
        //VANILLA (adds stored block render)
        if(insn.getOpcode() == INVOKESTATIC && insn.getPrevious().getOpcode() == ACONST_NULL) {
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 11));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //iblockstate variable
            list.add(new VarInsnNode(ALOAD, 15));
            //world variable
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/client/renderer/chunk/RenderChunk", (obfuscated ? "field_189564_r" : "worldView"), "Lnet/minecraft/world/ChunkCache;"));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 14));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "renderFluidloggedBlock", "([ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V", false));

            instructions.insert(insn, list);
            return true;
        }

        return false;
    }
}
