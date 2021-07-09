package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 *
 * @author jbred
 *
 */
public class ItemMinecartPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //dispenseStack, lines 35, 52, 57
        if(ASMUtils.checkMethod(method, obfuscated ? "func_82487_b" : "dispenseStack", null)) return true;
        //onItemUse, line
        return ASMUtils.checkMethod(method, obfuscated ? "func_180614_a" : "onItemUse", null);
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
