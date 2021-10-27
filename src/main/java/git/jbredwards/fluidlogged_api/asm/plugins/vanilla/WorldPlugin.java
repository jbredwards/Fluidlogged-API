package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * corrects a lot of fluid related interactions
 * @author jbred
 *
 */
public class WorldPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //setBlockState, line 401
        if(checkMethod(method, obfuscated ? "func_180501_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z")) {
            return 1;
        }
        //neighborChanged

        //changes some methods to use FluidloggedUtils#getFluidOrReal
        if(checkMethod(method, obfuscated ? "func_72953_d" : "containsAnyLiquid", null)
        || checkMethod(method, obfuscated ? "func_147470_e" : "isFlammableWithin", null)
        || checkMethod(method, obfuscated ? "func_72918_a" : "handleMaterialAcceleration", null)
        || checkMethod(method, obfuscated ? "func_72875_a" : "isMaterialInBB", null)
        || checkMethod(method, obfuscated ? "func_175696_F" : "isWater", null)) return 3;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //setBlockState, line 401

        return false;
    }
}
