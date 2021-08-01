package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * fixes iblockstate issue with fluidlogged walls
 * @author jbred
 *
 */
public class BlockWallPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //canConnectTo
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176253_e" : "canConnectTo", null)) {
            currentMethod = 1;
            return true;
        }
        //getActualState
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176221_a" : "getActualState", null)) {
            currentMethod = 2;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //canConnectTo, line 128
        if(currentMethod == 1 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return true;
        }
        //getActualState, line 193
        if(currentMethod == 2 && ASMUtils.checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock", null)) {
            instructions.insert(insn, method("isAirBlock", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.remove(insn);
            //finish all transformations
            finishedAll = true;
            return true;
        }

        return false;
    }
}
