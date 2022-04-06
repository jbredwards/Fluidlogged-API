package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * FluidStates now get ticked
 * @author jbred
 *
 */
public final class PluginWorldServer implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        //updateBlocks
        if(checkMethod(method, obfuscated ? "func_147456_g" : "updateBlocks", null))
            return 1;
        //updateBlockTick
        else if(checkMethod(method, obfuscated ? "func_175654_a" : "updateBlockTick", null))
            return 2;
        //tickUpdates
        else if(checkMethod(method, obfuscated ? "func_72955_a" : "tickUpdates", null))
            return 3;

        else return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //updateBlocks, line 500
        if(index == 1 && checkMethod(insn, obfuscated ? "func_76319_b" : "endSection", "()V")) {
            final InsnList list = new InsnList();
            //new BlockPos instance
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
            list.add(genMethodNode("updateBlocks", "(Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;)V"));
            //add new code
            instructions.insert(insn, list);
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);
            return true;
        }
        //updateBlockTick, line 571
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            //gets the compared block param
            list.add(new VarInsnNode(ALOAD, 2));
            //adds the new code
            list.add(genMethodNode("updateBlockTick", "(Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        //tickUpdates, line 769
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_149680_a" : "isEqualTo", null)) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 4));
            list.add(genMethodNode("tickUpdates", "(ZLnet/minecraft/world/WorldServer;Lnet/minecraft/world/NextTickListEntry;)Z"));

            instructions.insert(insn, list);
            return true;
        }

        return false;
    }
}
