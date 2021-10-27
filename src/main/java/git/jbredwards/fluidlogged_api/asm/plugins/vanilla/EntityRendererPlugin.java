package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes underwater block selection
 * for real though, it's hard-coded to not render underwater, but if I remove that, works fine underwater, WHY?! lol
 * @author jbred
 *
 */
public final class EntityRendererPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, obfuscated ? "func_175068_a" : "renderWorldPass", "(IFJ)V") ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //removes '!entity.isInsideOfMaterial()' at 1409 and replaces it with '!false'
        if(checkMethod(insn, obfuscated ? "func_70055_a" : "isInsideOfMaterial", "(Lnet/minecraft/block/material/Material;)Z")) {
            instructions.insert(insn, new InsnNode(ICONST_0));
            instructions.remove(getPrevious(insn, 2));
            instructions.remove(getPrevious(insn, 1));
            instructions.remove(getPrevious(insn, 0));

            return true;
        }

        return false;
    }
}
