package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * fixes inconsistent fluidlogged block step sound & fall damage updates
 * @author jbred
 *
 */
public class EntityPlugin extends AbstractMultiMethodPlugin
{
    public int iteration = 0;

    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //move
        if(ASMUtils.checkMethod(method, obfuscated ? "func_70091_d" : "move", null)) {
            currentMethod = 1;
            return true;
        }
        //playStepSound
        if(ASMUtils.checkMethod(method, obfuscated ? "func_180429_a" : "playStepSound", null)) {
            currentMethod = 2;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //move, line 996
        if(currentMethod == 1 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null) && ++iteration == 2) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return true;
        }
        //playStepSound, line 1187
        if(currentMethod == 2 && ASMUtils.checkMethod(insn, obfuscated ? "func_76224_d" : "isLiquid", "()Z")) {
            //new code
            instructions.insert(insn, method("playStepSound", "(Lnet/minecraft/block/Block;)Z"));
            //remove old
            instructions.remove(ASMUtils.getPrevious(insn, 2));
            instructions.remove(ASMUtils.getPrevious(insn, 1));
            instructions.remove(ASMUtils.getPrevious(insn, 0));
            //finish transformation
            finishedAll = true;
            return true;
        }

        return false;
    }
}
