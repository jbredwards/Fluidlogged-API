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
        //updateBlocks
        if(ASMUtils.checkMethod(method, obfuscated ? "func_147456_g" : "updateBlocks", null)) {
            currentMethod = 1;
            return true;
        }
        //updateBlockTick
        if(ASMUtils.checkMethod(method, obfuscated ? "func_175654_a" : "updateBlockTick", null)) {
            currentMethod = 2;
            return true;
        }
        //tickUpdates
        if(ASMUtils.checkMethod(method, obfuscated ? "func_72955_a" : "tickUpdates", null)) {
            currentMethod = 3;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //updateBlocks, line 491
        if(currentMethod == 1 && ASMUtils.checkMethod(insn, obfuscated ? "func_177485_a" : "get", "(III)Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            //this param
            list.add(new VarInsnNode(ALOAD, 0));
            //new BlockPos
            list.add(new TypeInsnNode(NEW, "net/minecraft/util/math/BlockPos"));
            list.add(new InsnNode(DUP));
            list.add(new VarInsnNode(ILOAD, 14));
            list.add(new VarInsnNode(ILOAD, 6));
            list.add(new InsnNode(IADD));
            list.add(new VarInsnNode(ILOAD, 16));
            list.add(new VarInsnNode(ALOAD, 11));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/chunk/storage/ExtendedBlockStorage", obfuscated ? "func_76662_d" : "getYLocation", "()I", false));
            list.add(new InsnNode(IADD));
            list.add(new VarInsnNode(ILOAD, 15));
            list.add(new VarInsnNode(ILOAD, 7));
            list.add(new InsnNode(IADD));
            list.add(new MethodInsnNode(INVOKESPECIAL, "net/minecraft/util/math/BlockPos", "<init>", "(III)V", false));
            //adds the new code
            list.add(method("getStoredOrReal", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            return true;
        }
        //updateBlockTick, line 571
        if(currentMethod == 2 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
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
        if(currentMethod == 3 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
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
