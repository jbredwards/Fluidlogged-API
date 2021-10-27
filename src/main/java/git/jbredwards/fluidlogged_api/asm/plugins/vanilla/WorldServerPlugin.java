package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * properly updates fluidlogged blocks
 * @author jbred
 *
 */
public final class WorldServerPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //updateBlockTick
        if(checkMethod(method, obfuscated ? "func_175654_a" : "updateBlockTick", null)) return 1;
        //tickUpdates
        return checkMethod(method, obfuscated ? "func_72955_a" : "tickUpdates", null) ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //(updateBlockTick, line 571) & (tickUpdates, line 767)
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            //gets the compared block
            list.add(new VarInsnNode(ALOAD, 2));
            //adds the new code
            list.add(genMethodNode("updateBlockTick", "(Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
