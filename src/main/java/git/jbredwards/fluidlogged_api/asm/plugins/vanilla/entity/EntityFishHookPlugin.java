package git.jbredwards.fluidlogged_api.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class EntityFishHookPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //onUpdate, line 174
        if(checkMethod(method, obfuscated ? "func_70071_h_" : "onUpdate", "()V"))
            return 1;

        //catchingFish, lines 428 & 478
        else if(checkMethod(method, obfuscated ? "func_190621_a" : "catchingFish", "(Lnet/minecraft/util/math/BlockPos;)V"))
            return 2;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/common/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return index == 1;
        }

        return false;
    }
}
