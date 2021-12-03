package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class BlockDynamicLiquidPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, obfuscated ? "func_180690_f" : "placeStaticBlock", null))
            return 1;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(index == 1 && checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
            instructions.insert(insn, genMethodNode("placeStaticBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
