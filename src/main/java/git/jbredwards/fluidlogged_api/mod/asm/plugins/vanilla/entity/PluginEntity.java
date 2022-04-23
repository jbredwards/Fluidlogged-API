package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class PluginEntity implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_180799_ab" : "isInLava")) return 1;
        else if(method.name.equals(obfuscated ? "func_70072_I" : "handleWaterMovement")) return 1;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //don't change the AABB prior to checking fluid collision
        if(index == 1 && checkMethod(insn, obfuscated ? "func_72314_b" : "grow")) {
            removeFrom(instructions, insn, -3);
            return true;
        }

        return false;
    }
}
