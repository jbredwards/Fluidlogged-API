package git.jbredwards.fluidlogged_api.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes console spam when registering a fluidlogged block with a fluid already bound to a block
 * @author jbred
 *
 */
public final class FluidPlugin implements IASMPlugin
{
    private boolean doFirst = true;

    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return method.name.equals("setBlock") ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //line 186
        if(doFirst && insn.getOpcode() == ALOAD && ((VarInsnNode)insn).var == 1) {
            instructions.insert(insn, genMethodNode("updateIfNotFluidloggable", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/Block;)Z"));
            doFirst = false;
        }
        //line 186
        else if(insn.getOpcode() == IF_ACMPNE) {
            instructions.insert(insn, new JumpInsnNode(IFEQ, ((JumpInsnNode)insn).label));
            instructions.remove(insn);
        }
        //line 192
        else if(insn.getOpcode() == INVOKEINTERFACE) {
            instructions.insert(insn, genMethodNode("fluidBlockErrorSpamFix", "(Lorg/apache/logging/log4j/Logger;Ljava/lang/String;Lnet/minecraft/block/Block;Ljava/lang/String;Lnet/minecraft/block/Block;)V"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
