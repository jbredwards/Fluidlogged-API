package git.jbredwards.fluidlogged_api.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * overrides the updateTick method to fix the following issues:
 * -vanilla fluids no longer do block mixing when they shouldn't
 * -vanilla fluids now flow from fluidlogged blocks
 * @author jbred
 *
 */
public final class BlockDynamicLiquidPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //updateTick, line 29
        return checkMethod(method, obfuscated ? "func_180650_b" : "updateTick", null) ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_175697_a" : "isAreaLoaded", "(Lnet/minecraft/util/math/BlockPos;I)Z")) {
            final InsnList list = new InsnList();
            //params
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 3));
            list.add(new VarInsnNode(ALOAD, 4));
            //adds new code
            list.add(genMethodNode("updateTick", "(Lnet/minecraft/block/BlockDynamicLiquid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)Z"));
            instructions.insert(insn, list);
            //remove old
            instructions.remove(insn.getPrevious());
            instructions.remove(insn.getPrevious());
            instructions.remove(insn.getPrevious());
            instructions.remove(insn.getPrevious());
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
