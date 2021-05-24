package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.util.FluidloggedUtils;
import net.minecraft.block.BlockStairs;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static net.minecraft.block.BlockStairs.FACING;
import static net.minecraft.block.BlockStairs.HALF;
import static org.objectweb.asm.Opcodes.*;

/**
 *
 * @author jbred
 *
 */
public class BlockStairsPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_176221_a" : "getActualState";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKESTATIC && insn instanceof MethodInsnNode) {
            ((MethodInsnNode)insn).owner = "git/jbredwards/fluidlogged/asm/plugin/BlockStairsPlugin";
            ((MethodInsnNode)insn).name = "getShape";

            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static BlockStairs.EnumShape getShape(IBlockState state, IBlockAccess world, BlockPos pos) {
        final EnumFacing face = state.getValue(FACING);
        IBlockState neighbor = FluidloggedUtils.getStoredOrReal(world, pos.offset(face));
        //if outer shape
        if(neighbor.getBlock() instanceof BlockStairs && state.getValue(HALF) == neighbor.getValue(HALF)) {
            final EnumFacing neighborFace = neighbor.getValue(FACING);

            if(neighborFace.getAxis() != face.getAxis() && isDifferentStairs(state, world, pos, neighborFace.getOpposite())) {
                if(neighborFace == face.rotateYCCW()) return BlockStairs.EnumShape.OUTER_LEFT;
                else return BlockStairs.EnumShape.OUTER_RIGHT;
            }
        }
        //if inner shape
        neighbor = FluidloggedUtils.getStoredOrReal(world, pos.offset(face.getOpposite()));
        if(neighbor.getBlock() instanceof BlockStairs && state.getValue(HALF) == neighbor.getValue(HALF)) {
            final EnumFacing neighborFace = neighbor.getValue(FACING);

            if(neighborFace.getAxis() != face.getAxis() && isDifferentStairs(state, world, pos, neighborFace)) {
                if(neighborFace == face.rotateYCCW()) return BlockStairs.EnumShape.INNER_LEFT;
                else return BlockStairs.EnumShape.INNER_RIGHT;
            }
        }

        //default return statement
        return BlockStairs.EnumShape.STRAIGHT;
    }

    public static boolean isDifferentStairs(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing offset) {
        final IBlockState neighbor = FluidloggedUtils.getStoredOrReal(world, pos.offset(offset));
        return !(neighbor.getBlock() instanceof BlockStairs) || state.getValue(FACING) != neighbor.getValue(FACING) || state.getValue(HALF) != neighbor.getValue(HALF);
    }
}
