package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 *
 * @author jbred
 *
 */
public class BlockChestPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //getBoundingBox
        if(ASMUtils.checkMethod(method, obfuscated ? "func_185496_a" : "getBoundingBox", null)) return true;
        //onBlockAdded
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176213_c" : "onBlockAdded", null)) return true;
        //onBlockPlacedBy
        if(ASMUtils.checkMethod(method, obfuscated ? "func_180633_a" : "onBlockPlacedBy", null)) return true;
        //checkForSurroundingChests
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176455_e" : "checkForSurroundingChests", null)) return true;
        //correctFacing
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176458_f" : "correctFacing", null)) return true;
        //canPlaceBlockAt
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176196_c" : "canPlaceBlockAt", null)) return true;
        //isDoubleChest
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176454_e" : "isDoubleChest", null)) return true;
        //getContainer
        if(ASMUtils.checkMethod(method, obfuscated ? "func_189418_a" : "getContainer", null)) return true;
        //getContainer
        return ASMUtils.checkMethod(method, obfuscated ? "func_189418_a" : "getContainer", null);
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //getBlockState
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
        }
        //setBlockState
        else if(ASMUtils.checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
            instructions.insert(insn, method("setStoredOrRealSimple", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
        }

        return false;
    }
}
