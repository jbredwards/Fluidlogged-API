package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * allows fence gates to not have glitched behavior when fluidlogged
 * @author jbred
 *
 */
public class BlockFenceGatePlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //getActualState
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176221_a" : "getActualState", null)) return true;
        //canPlaceBlockAt
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176196_c" : "canPlaceBlockAt", null)) return true;
        //onBlockActivated
        if(ASMUtils.checkMethod(method, obfuscated ? "func_180639_a" : "onBlockActivated", null)) return true;
        //neighborChanged
        if(ASMUtils.checkMethod(method, obfuscated ? "func_189540_a" : "neighborChanged", null)) return true;
        //canBeConnectedTo
        return ASMUtils.checkMethod(method, "canBeConnectedTo", null);
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //world.getBlockState to FluidloggedUtils.getStoredOrReal
        if(insn.getOpcode() == INVOKEINTERFACE && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
        }
        //world.setBlockState to BlockFenceGatePlugin.set
        else if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
            instructions.insert(insn, method("setStoredOrRealSimple", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
        }

        return false;
    }
}
