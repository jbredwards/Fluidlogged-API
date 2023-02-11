package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.BlockDoor;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * update upper FluidState & correct canFluidFlow
 * @author jbred
 *
 */
public final class PluginBlockDoor implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_180639_a" : "onBlockActivated")
                || method.name.equals(obfuscated ? "func_176512_a" : "toggleDoor")
                || method.name.equals(obfuscated ? "func_189540_a" : "neighborChanged")) return 1;

        else return method.name.equals(obfuscated ? "func_176208_a" : "onBlockHarvested") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * onBlockActivated, toggleDoor, neighborChanged: (changes are around lines 178, 197, and 265)
         * Old code:
         * worldIn.markBlockRangeForRenderUpdate(blockpos, pos);
         *
         * New code:
         * //update upper FluidState when the door opens or closes
         * Hooks.notifyDoorFluids(worldIn, blockpos, pos);
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_175704_b" : "markBlockRangeForRenderUpdate", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V")) {
            instructions.insert(insn, genMethodNode("notifyDoorFluids", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V"));
            instructions.remove(insn);
            return true;
        }

        else if(index == 2) {
            /*
            * onBlockHarvested: (changes are around line 370)
            * Old code:
            * if (player.capabilities.isCreativeMode)
            * {
            *     ...
            * }
            *
            * New code:
            * //don't void fluids here if the player breaking this is in creative
            * if (false)
            * {
            *     ...
            * }
            */
            if(checkField(insn, obfuscated ? "field_75098_d" : "isCreativeMode") && getNext(insn, 3) instanceof LineNumberNode) {
                instructions.insert(insn, new InsnNode(ICONST_0));
                removeFrom(instructions, insn, -2);
            }
            /*
             * onBlockHarvested: (changes are around lines 365 & 375)
             * Old code:
             * worldIn.setBlockToAir(blockpos1);
             *
             * New code:
             * //don't notify neighboring blocks of an update
             * Hooks.setBlockToAirNoUpdate(worldIn, blockpos1);
             */
            else if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
                final boolean isLast = getNext(insn, 5).getOpcode() == RETURN;
                instructions.insertBefore(insn, genMethodNode("setBlockToAirNoUpdate", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V"));
                removeFrom(instructions, insn, 1);
                return isLast;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        /*
         * IFluidloggable:
         * New code:
         * //ensure FluidStates only flow from certain sides
         * @ASMGenerated
         * public boolean canFluidFlow(IBlockAccess world, BlockPos pos, IBlockState here, EnumFacing side)
         * {
         *     return Hooks.canDoorFluidFlow(world, pos, here, side);
         * }
         */
        addMethod(classNode, "canFluidFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z",
            "canDoorFluidFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canDoorFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull EnumFacing side) {
            if(side.getAxis().isVertical()) return true;
            here = here.getActualState(world, pos);
            final EnumFacing facing = here.getValue(BlockDoor.FACING);

            return (here.getValue(BlockDoor.OPEN) ? (here.getValue(BlockDoor.HINGE) == BlockDoor.EnumHingePosition.RIGHT
                    ? facing.rotateY() : facing.rotateYCCW()) : facing.getOpposite()) != side;
        }

        public static void notifyDoorFluids(@Nonnull World world, @Nonnull BlockPos rangeMin, @Nonnull BlockPos rangeMax) {
            FluidloggedUtils.notifyFluids(world, rangeMin.up(), FluidState.get(world, rangeMin.up()), false, EnumFacing.DOWN);
            world.markBlockRangeForRenderUpdate(rangeMin, rangeMax);
        }

        public static void setBlockToAirNoUpdate(@Nonnull World world, @Nonnull BlockPos pos) {
            world.setBlockState(pos, FluidState.get(world, pos).getState(), 2);
        }
    }
}
