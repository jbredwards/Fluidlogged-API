package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 *
 * @author jbred
 *
 */
public class BlockRailBasePlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //isRailBlock
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176562_d" : "isRailBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z")) {
            currentMethod = 1;
            return true;
        }
        //constructor
        if(ASMUtils.checkMethod(method, "<init>", "(Z)V")) {
            currentMethod = 2;
            return true;
        }
        //neighborChanged
        if(ASMUtils.checkMethod(method, obfuscated ? "func_189540_a" : "neighborChanged", null)) {
            currentMethod = 3;
            return true;
        }
        //rotateBlock
        if(ASMUtils.checkMethod(method, "rotateBlock", null)) {
            currentMethod = 4;
            return true;
        }
        //findRailAt
        if(ASMUtils.checkMethod(method, obfuscated ? "func_180697_b" : "findRailAt", null)) {
            currentMethod = 5;
            return true;
        }
        //connectTo
        if(ASMUtils.checkMethod(method, obfuscated ? "func_150645_c" : "connectTo", null)) {
            currentMethod = 6;
            return true;
        }
        //place
        if(ASMUtils.checkMethod(method, obfuscated ? "func_180364_a" : "place", null)) {
            currentMethod = 7;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //isRailBlock, line 31
        if(currentMethod == 1 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return true;
        }
        //constructor, line 42
        if(currentMethod == 2 && insn.getOpcode() == GETSTATIC) {
            instructions.insert(insn, new FieldInsnNode(GETSTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedConstants", "RAIL", "Lnet/minecraft/block/material/Material;"));
            instructions.remove(insn);
            return true;
        }
        //neighborChanged
        if(currentMethod == 3) {
            //line 119
            if(ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null) && insn.getPrevious().getOpcode() == ALOAD) {
                instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
                instructions.remove(insn);
                return false;
            }
            //line 148
            if(ASMUtils.checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir", null)) {
                instructions.insert(insn, method("setBlockToAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
                instructions.remove(insn);
                return true;
            }
        }
        //rotateBlock
        if(currentMethod == 4) {
            //line 227
            if(ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
                instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
                instructions.remove(insn);
                return false;
            }
            //line 282
            if(ASMUtils.checkMethod(insn, obfuscated ? "func_175656_a" : "setBlockState", null)) {
                instructions.insert(insn, method("setStoredOrRealSimple", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z"));
                instructions.remove(insn);
                finishedAll = true;
                return true;
            }
        }
        //findRailAt, lines 453, 462, 471
        if(currentMethod == 5 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return false;
        }
        //connectTo, line 598
        if(currentMethod == 6 && ASMUtils.checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
            instructions.insert(insn, method("setStoredOrRealSimple", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
            return true;
        }
        //place
        if(currentMethod == 7) {
            //line 756
            if(ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
                instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
                instructions.remove(insn);
                return false;
            }
            //line 758
            if(ASMUtils.checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
                instructions.insert(insn, method("setStoredOrRealSimple", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
                instructions.remove(insn);
                finishedAll = true;
                return true;
            }
        }

        return false;
    }
}
