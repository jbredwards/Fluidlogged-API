package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes all issues with fluidlogged z-fighting
 * @author jbred
 *
 */
public final class ModelFluidPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, "lambda$buildQuads$13", "(II)F")) return 1;
        if(checkMethod(method, "lambda$buildQuads$11", "(II)F")) return 2;
        return checkMethod(method, "lambda$buildQuads$7", "(I)F") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //fixes z
        if(index == 1 && insn.getOpcode() == I2F) {
            final InsnList list = new InsnList();
            //si local var
            list.add(new VarInsnNode(ILOAD, 0));
            //method
            list.add(genMethodNode("fixTextureFightingZ", "(FI)F"));

            instructions.insert(insn, list);
            return true;
        }
        //fixes x
        if(index == 2 && insn.getOpcode() == I2F) {
            final InsnList list = new InsnList();
            //si local var
            list.add(new VarInsnNode(ILOAD, 0));
            //method
            list.add(genMethodNode("fixTextureFightingX", "(FI)F"));

            instructions.insert(insn, list);
            return true;
        }
        //fixes y
        if(index == 3) {
            //gas
            if(insn.getOpcode() == FCONST_1) {
                instructions.insert(insn, new LdcInsnNode(new Float("0.998")));
                instructions.remove(insn);
                return false;
            }
            //normal
            if(insn.getOpcode() == FCONST_0) {
                instructions.insert(insn, new LdcInsnNode(new Float("0.002")));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }
}
