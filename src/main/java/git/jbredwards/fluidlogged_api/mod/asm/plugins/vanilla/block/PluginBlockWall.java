package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.getFluidFromState;

/**
 * makes walls fluidloggable by default
 * @author jbred
 *
 */
public final class PluginBlockWall implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_176221_a" : "getActualState"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getActualState (changes are around line 209):
         * Old code:
         * return state.withProperty(UP, Boolean.valueOf(!flag4 || !worldIn.isAirBlock(pos.up()))).withProperty(NORTH, Boolean.valueOf(flag)).withProperty(EAST, Boolean.valueOf(flag1)).withProperty(SOUTH, Boolean.valueOf(flag2)).withProperty(WEST, Boolean.valueOf(flag3));
         *
         * New code:
         * //don't have up post if the block above is a fluid
         * return state.withProperty(UP, Boolean.valueOf(!flag4 || !Hooks.isAirOrFluid(worldIn, pos.up()))).withProperty(NORTH, Boolean.valueOf(flag)).withProperty(EAST, Boolean.valueOf(flag1)).withProperty(SOUTH, Boolean.valueOf(flag2)).withProperty(WEST, Boolean.valueOf(flag3));
         */
        if(checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock", "(Lnet/minecraft/util/math/BlockPos;)Z")) {
            instructions.insert(insn, genMethodNode("isAirOrFluid", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isAirOrFluid(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            final IBlockState here = world.getBlockState(pos);
            return here.getBlock().isAir(here, world, pos) || getFluidFromState(here) != null;
        }
    }
}
