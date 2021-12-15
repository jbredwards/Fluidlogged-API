package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class BlockSkullPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, obfuscated ? "func_176500_f" : "checkWitherSpawn", null) ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //dropBlock, line 153
        if(checkField(insn, obfuscated ? "field_150350_a" : "AIR", "Lnet/minecraft/block/Block;")) {
            final InsnList list = new InsnList();
            //parameters
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            //adds new code
            list.add(genMethodNode("getFluidState", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insertBefore(insn, list);
            instructions.remove(insn.getNext());
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
