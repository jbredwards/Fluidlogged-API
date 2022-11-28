package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockStairs;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * update neighboring fluids when this changes shape
 * @author jbred
 *
 */
public final class PluginBlockStairs implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        /*
         * New code:
         * //update neighboring fluids when this changes shape
         * @ASMGenerated
         * public void neighborChanged(IBlockState state, World worldIn, BlockPos pos, Block blockIn, BlockPos fromPos)
         * {
         *     Hooks.notifyNeighboringFluids(worldIn, pos, blockIn, fromPos);
         * }
         */
        addMethod(classNode, obfuscated ? "func_189540_a" : "neighborChanged", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;Lnet/minecraft/util/math/BlockPos;)V",
            "notifyNeighboringFluids", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;Lnet/minecraft/util/math/BlockPos;)V", generator -> {
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(ALOAD, 5);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void notifyNeighboringFluids(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Block blockIn, @Nonnull BlockPos fromPos) {
            if(pos.getY() == fromPos.getY() && BlockStairs.isBlockStairs(blockIn.getDefaultState()))
                FluidloggedUtils.notifyFluids(world, pos, FluidState.get(world, pos), false, EnumFacing.UP, EnumFacing.DOWN);
        }
    }
}
