package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;

/**
 *
 * @author jbred
 *
 */
public final class RenderChunkPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_178581_b" : "rebuildChunk";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(FFFLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;)V";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //OPTIFINE
        if(insn.getOpcode() == INVOKESTATIC && insn instanceof MethodInsnNode && ((MethodInsnNode)insn).name.equals("callVoid")) {
            if(insn.getPrevious().getPrevious().getOpcode() == ACONST_NULL) {
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
                //block variable
                list.add(new VarInsnNode(ALOAD, 19));
                //chunkCacheOF variable
                list.add(new VarInsnNode(ALOAD, 11));
                //block position variable
                list.add(new VarInsnNode(ALOAD, 17));
                //chunk position variable
                list.add(new VarInsnNode(ALOAD, 7));
                //actually adds the new code
                list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/ASMHooks", "renderFluidloggedBlockOF", "([ZLnet/minecraft/client/renderer/chunk/RenderChunk;Lnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V", false));

                instructions.insert(insn, list);
                return true;
            }
        }
        //VANILLA
        if(insn.getOpcode() == INVOKESTATIC && insn.getPrevious().getOpcode() == ACONST_NULL) {
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 11));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //block variable
            list.add(new VarInsnNode(ALOAD, 16));
            //world variable
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/client/renderer/chunk/RenderChunk", (obfuscated ? "field_189564_r" : "worldView"), "Lnet/minecraft/world/ChunkCache;"));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 14));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/ASMHooks", "renderFluidloggedBlock", "([ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V", false));

            instructions.insert(insn, list);
            return true;
        }

        return false;
    }
}
