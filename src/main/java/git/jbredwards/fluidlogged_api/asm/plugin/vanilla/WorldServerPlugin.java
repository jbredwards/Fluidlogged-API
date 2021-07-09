package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * properly updates fluidlogged blocks
 * @author jbred
 *
 */
public class WorldServerPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //updateBlockTick
        if(ASMUtils.checkMethod(method, obfuscated ? "func_175654_a" : "updateBlockTick", null)) {
            currentMethod = 1;
            return true;
        }
        //tickUpdates
        if(ASMUtils.checkMethod(method, obfuscated ? "func_72955_a" : "tickUpdates", null)) {
            currentMethod = 2;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //updateBlockTick, line 571
        if(currentMethod == 1 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            //gets the compared block
            list.add(new VarInsnNode(ALOAD, 2));
            //adds the new code
            list.add(method("correctStoredOrLiquid", "(Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        //tickUpdates, line 767
        if(currentMethod == 2 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            //gets the compared block
            list.add(new VarInsnNode(ALOAD, 4));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/NextTickListEntry", obfuscated ? "func_151351_a" : "getBlock", "()Lnet/minecraft/block/Block;", false));
            //adds the new code
            list.add(method("correctStoredOrLiquid", "(Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            finishedAll = true;
            return true;
        }

        return false;
    }
}
