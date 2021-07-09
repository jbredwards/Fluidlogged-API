package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;

/**
 * fixes underwater block selection
 * for real though, it's hard-coded to not render underwater, but if I remove that, works fine underwater, WHY?! lol
 * @author jbred
 *
 */
public final class EntityRendererPlugin extends AbstractPlugin
{
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_175068_a" : "renderWorldPass";
    }

    @Nullable
    @Override
    public String getMethodDesc() {
        return "(IFJ)V";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //removes '!entity.isInsideOfMaterial()' at 1409 and replaces it with '!false'
        if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_70055_a" : "isInsideOfMaterial", "(Lnet/minecraft/block/material/Material;)Z")) {
            //ICONST_0 can also refer to 'false'
            instructions.insert(insn, new InsnNode(ICONST_0));
            instructions.remove(ASMUtils.getPrevious(insn, 2));
            instructions.remove(ASMUtils.getPrevious(insn, 1));
            instructions.remove(ASMUtils.getPrevious(insn, 0));

            return true;
        }

        return false;
    }
}
