package git.jbredwards.fluidlogged_api.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes drain interactions across all modded fluids & FluidStates
 * @author jbred
 *
 */
public final class BlockSpongePlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, obfuscated ? "func_176311_e" : "tryAbsorb", null) ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_176312_d" : "absorb", null)) {
            instructions.insert(insn, genMethodNode("absorb", "(Lnet/minecraft/block/BlockSponge;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
