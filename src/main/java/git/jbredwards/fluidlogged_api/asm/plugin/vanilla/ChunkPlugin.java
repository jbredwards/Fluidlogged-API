package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 *
 * @author jbred
 *
 */
public class ChunkPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //setBlockState
        if(ASMUtils.checkMethod(method, obfuscated ? "func_177436_a" : "setBlockState", null)) {
            method.maxStack++;
            return true;
        }
        //read
        return ASMUtils.checkMethod(method, obfuscated ? "func_186033_a" : "read", "(Lnet/minecraft/network/PacketBuffer;IZ)V");
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, "shouldRefresh", null)) {
            instructions.insert(insn, method("shouldRefresh", "(Lnet/minecraft/tileentity/TileEntity;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/chunk/Chunk;)Z"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
        }

        return false;
    }
}
