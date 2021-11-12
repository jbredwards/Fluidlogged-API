package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

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
        //setBlockToAir, line 456
        if(checkMethod(method, obfuscated ? "func_175698_g" : "setBlockToAir", null)) return 2;
        //destroyBlock, line 480
        if(checkMethod(method, obfuscated ? "func_175655_b" :  "destroyBlock", null)) return 2;

        //neighborChanged

        //changes some methods to use FluidloggedUtils#getFluidOrReal
        if(checkMethod(method, obfuscated ? "func_72953_d" : "containsAnyLiquid", null)
        || checkMethod(method, obfuscated ? "func_147470_e" : "isFlammableWithin", null)
        || checkMethod(method, obfuscated ? "func_72918_a" : "handleMaterialAcceleration", null)
        || checkMethod(method, obfuscated ? "func_72875_a" : "isMaterialInBB", null)
        || checkMethod(method, obfuscated ? "func_175696_F" : "isWater", null)) return 4;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //setBlockState, line 401

        //setBlockToAir & destroyBlock
        if(index == 2 && checkMethod(insn, obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(genMethodNode("getFluidState", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));

            instructions.insert(insn, list);
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
