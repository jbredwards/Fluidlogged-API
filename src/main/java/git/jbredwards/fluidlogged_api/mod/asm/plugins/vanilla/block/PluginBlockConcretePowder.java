package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.canFluidFlow;
import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.getFluidOrReal;

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
        /*
         * onEndFalling: (changes are around line 32)
         * Old code:
         * if (hitState.getMaterial().isLiquid())
         * {
         *     ...
         * }
         *
         * New code:
         * //check for FluidState
         * if (FluidloggedUtils.getFluidOrReal(worldIn, pos, hitState).getMaterial().isLiquid())
         * {
         *     ...
         * }
         */
        if(index == 1 && insn.getOpcode() == ALOAD) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            return true;
        }
        /*
         * tryTouchWater: (changes are around line 48)
         * Old code:
         * if (worldIn.getBlockState(blockpos).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         *
         * New code:
         * //
         * if (Hooks.tryTouchWater(worldIn, blockpos, enumfacing)))
         * {
         *     ...
         * }
         */
        else if(index == 2 && insn.getOpcode() == INVOKEINTERFACE) {
            //EnumFacing local var
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 8));
            //add new
            instructions.insertBefore(insn, genMethodNode("tryTouchWater", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"));
            ((JumpInsnNode)getNext(insn, 3)).setOpcode(IFEQ);
            //remove old
            removeFrom(instructions, insn, 2);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean tryTouchWater(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumFacing facing) {
            final IBlockState state = world.getBlockState(pos);
            return getFluidOrReal(world, pos, state).getMaterial() == Material.WATER
                    && canFluidFlow(world, pos, state, facing.getOpposite());
        }
    }
}
