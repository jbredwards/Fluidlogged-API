package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * concrete forms from concrete powder while its next to flowing water FluidStates
 * @author jbred
 *
 */
public final class PluginBlockConcretePowder implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_176502_a_" : "onEndFalling")) return 1;
        else return method.name.equals(obfuscated ? "func_192425_e" : "tryTouchWater") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //onEndFalling, look for a FluidState rather than just the old IBlockState
        if(index == 1 && insn.getOpcode() == ALOAD) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            return true;
        }
        //tryTouchWater, line 48
        else if(index == 2 && insn.getOpcode() == IF_ACMPNE) {
            final InsnList list = new InsnList();
            //EnumFacing local var
            list.add(new VarInsnNode(ALOAD, 8));
            //add new
            list.add(genMethodNode("tryTouchWater", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"));
            //remove old
            instructions.remove(getPrevious(insn, 3));
            instructions.remove(getPrevious(insn, 2));
            instructions.remove(getPrevious(insn, 1));
            instructions.insertBefore(insn, list);
            ((JumpInsnNode)insn).setOpcode(IFEQ);
            return true;
        }

        return false;
    }
}
