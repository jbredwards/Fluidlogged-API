package git.jbredwards.fluidlogged_api.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * vanilla FluidState drain & fill interactions now behave how you'd expect
 * @author jbred
 *
 */
public final class BlockLiquidWrapperPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //getTankProperties, line 62
        if(checkMethod(method, "getTankProperties", "()[Lnet/minecraftforge/fluids/capability/IFluidTankProperties;"))
            return 1;

        //fill, line 83
        else if(checkMethod(method, "fill", "(Lnet/minecraftforge/fluids/FluidStack;Z)I"))
            return 2;

        //drain, lines 98 & 106
        else if(checkMethod(method, "drain", "(Lnet/minecraftforge/fluids/FluidStack;Z)Lnet/minecraftforge/fluids/FluidStack;"))
            return 3;

        //drain, lines 124 & 132
        else if(checkMethod(method, "drain", "(IZ)Lnet/minecraftforge/fluids/FluidStack;"))
            return 3;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //getTankProperties, line 62
        if(index == 1 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/common/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        //fill, line 83
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
            instructions.insert(insn, genMethodNode("fillBlockLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
            return true;
        }
        //drain
        else if(index == 3) {
            //lines 98 & 124
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
                instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/common/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
                return false;
            }
            //lines 106 & 132
            else if(checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
                instructions.insert(insn, genMethodNode("drainBlockLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }
}
