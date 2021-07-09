package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 *
 * @author jbred
 *
 */
public class BlockRailPoweredPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //isSameRailWithPower
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176567_a" : "isSameRailWithPower", null)) {
            currentMethod = 1;
            return true;
        }
        //updateState
        if(ASMUtils.checkMethod(method, obfuscated ? "func_189541_b" : "updateState", null)) {
            currentMethod = 2;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //isSameRailWithPower
        if(currentMethod == 1 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return true;
        }
        //updateState
        if(currentMethod == 2 && ASMUtils.checkMethod(insn, obfuscated ? "func_175656_a" : "setBlockState", null)) {
            instructions.insert(insn, method("setStoredOrRealSimple", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
            finishedAll = true;
            return true;
        }

        return false;
    }
}
