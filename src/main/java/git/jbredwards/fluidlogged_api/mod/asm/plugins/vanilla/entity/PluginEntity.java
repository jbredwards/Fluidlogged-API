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
        else if(method.name.equals(obfuscated ? "func_71061_d_" : "doWaterSplashEffect")) return 2;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //don't change the AABB prior to checking fluid collision
        if(index == 1 && checkMethod(insn, obfuscated ? "func_72314_b" : "grow")) {
            removeFrom(instructions, insn, -3);
            return true;
        }
        //doWaterSplashEffect
        else if(index == 2) {
            if(checkMethod(insn, obfuscated ? "func_76128_c" : "floor")) {
                //add motion to position the particles better
                instructions.insertBefore(insn, new InsnNode(D2F));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/entity/Entity", obfuscated ? "field_70181_x" : "motionY", "D"));
                instructions.insertBefore(insn, new InsnNode(D2F));
                instructions.insertBefore(insn, new LdcInsnNode(-0.7f));
                instructions.insertBefore(insn, new InsnNode(FMUL));
                instructions.insertBefore(insn, new InsnNode(FADD));
                instructions.insertBefore(insn, new LdcInsnNode(0.1f));
                instructions.insertBefore(insn, new InsnNode(FSUB));
                //don't round y value, not needed
                removeFrom(instructions, insn, 1);
            }
            //remove offset, not needed
            else if(insn.getOpcode() == FADD && insn.getPrevious().getOpcode() == FCONST_1)
                removeFrom(instructions, insn, -1);
            //remove bubble particle motion
            else if(checkField(insn, "WATER_BUBBLE")) {
                removeFrom(instructions, getNext(insn, 15), 12);
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
            }
        }

        return false;
    }
}
