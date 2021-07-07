package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * fixes fluidlogged trapdoor interaction
 * @author jbred
 *
 */
public final class BlockTrapDoorPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //onBlockActivated
        if(ASMUtils.checkMethod(method, obfuscated ? "func_180639_a" : "onBlockActivated", null)) return true;
        //neighborChanged
        else return ASMUtils.checkMethod(method, obfuscated ? "func_189540_a" : "neighborChanged", null);
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, null, "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z")) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "setStoredOrRealSimple", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z", false));
            instructions.remove(insn);
        }

        return false;
    }
}
