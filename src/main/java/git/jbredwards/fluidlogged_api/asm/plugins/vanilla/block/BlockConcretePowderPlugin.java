package git.jbredwards.fluidlogged_api.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class BlockConcretePowderPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //tryTouchWater, line 48
        return checkMethod(method, obfuscated ? "func_192425_e" : "tryTouchWater", null) ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(insn.getOpcode() == IF_ACMPNE) {
            final InsnList list = new InsnList();
            //EnumFacing local var
            list.add(new VarInsnNode(ALOAD, 8));
            //add new
            list.add(genMethodNode("tryTouchWater", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"));
            //remove old
            instructions.remove(getPrevious(insn, 3));
            instructions.remove(getPrevious(insn, 2));
            instructions.remove(getPrevious(insn, 1));
            instructions.insertBefore(insn, list);
            ((JumpInsnNode)insn).setOpcode(IFEQ);
            return true;
        }

        return false;
    }
}
