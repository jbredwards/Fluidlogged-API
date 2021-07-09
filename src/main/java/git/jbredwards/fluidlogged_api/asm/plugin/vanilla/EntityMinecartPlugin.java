package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * minecarts behave correctly with fluidlogged rails
 * @author jbred
 *
 */
public class EntityMinecartPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //onUpdate, line 379
        if(ASMUtils.checkMethod(method, obfuscated ? "func_70071_h_" : "onUpdate", "()V")) return true;
        //getPosOffset, line 760
        if(ASMUtils.checkMethod(method, obfuscated ? "func_70495_a" : "getPosOffset", null)) return true;
        //getPos, line 810
        if(ASMUtils.checkMethod(method, obfuscated ? "func_70489_a" : "getPos", null)) return true;
        //getMaxSpeed, line 1162
        return ASMUtils.checkMethod(method, "getMaxSpeed", "()D");
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
