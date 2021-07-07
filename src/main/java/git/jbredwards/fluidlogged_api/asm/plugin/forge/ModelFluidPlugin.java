package git.jbredwards.fluidlogged_api.asm.plugin.forge;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * fixes all issues with fluidlogged z-fighting
 * @author jbred
 *
 */
public class ModelFluidPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        if(ASMUtils.checkMethod(method, "lambda$buildQuads$13", "(II)F")) {
            currentMethod = 1;
            return true;
        }
        if(ASMUtils.checkMethod(method, "lambda$buildQuads$11", "(II)F")) {
            currentMethod = 2;
            return true;
        }
        if(ASMUtils.checkMethod(method, "lambda$buildQuads$7", "(I)F")) {
            currentMethod = 3;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //fixes z
        if(currentMethod == 1 && insn.getOpcode() == I2F) {
            final InsnList list = new InsnList();
            //si local var
            list.add(new VarInsnNode(ILOAD, 0));
            //method
            list.add(method("fixTextureFightingZ", "(FI)F"));

            instructions.insert(insn, list);
            return true;
        }
        //fixes x
        if(currentMethod == 2 && insn.getOpcode() == I2F) {
            final InsnList list = new InsnList();
            //si local var
            list.add(new VarInsnNode(ILOAD, 0));
            //method
            list.add(method("fixTextureFightingX", "(FI)F"));

            instructions.insert(insn, list);
            return true;
        }
        //fixes y
        if(currentMethod == 3) {
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

                finishedAll = true;
                return true;
            }
        }

        return false;
    }
}
