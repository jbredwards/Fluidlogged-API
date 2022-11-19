package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Random;

/**
 * update FluidStates
 * @author jbred
 *
 */
public final class PluginBlockStaticLiquid implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * updateLiquid:
         * New code:
         * //don't destroy the block here if fluidlogged
         * private void updateLiquid(World worldIn, BlockPos pos, IBlockState state)
         * {
         *     Hooks.updateLiquid(worldIn, pos, state);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176370_f" : "updateLiquid"),
            "updateLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)V", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );

        //change existing updateTick method to randomTick
        for(MethodNode method : classNode.methods) {
            if(method.name.equals(obfuscated ? "func_180650_b" : "updateTick")) {
                method.name = obfuscated ? "func_180645_a" : "randomTick";
                break;
            }
        }

        /*
         * updateTick:
         * New code:
         * //use BlockDynamicLiquid behavior if fluidlogged
         * @ASMGenerated
         * public void updateTick(World worldIn, BlockPos pos, IBlockState state, Random rand)
         * {
         *     Hooks.updateTick(worldIn, pos, state, rand);
         * }
         */
        addMethod(classNode, obfuscated ? "func_180650_b" : "updateTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V",
            "updateTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void updateLiquid(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
            if(!FluidState.get(worldIn, pos).isEmpty()) worldIn.scheduleUpdate(pos, state.getBlock(), state.getBlock().tickRate(worldIn));
            else {
                final BlockLiquid block = BlockLiquid.getFlowingBlock(state.getMaterial());
                worldIn.setBlockState(pos, block.getDefaultState().withProperty(BlockLiquid.LEVEL, state.getValue(BlockLiquid.LEVEL)), 2);
                worldIn.scheduleUpdate(pos, block, block.tickRate(worldIn));
            }
        }

        public static void updateTick(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
            if(!FluidState.get(worldIn, pos).isEmpty()) {
                final BlockLiquid block = BlockLiquid.getFlowingBlock(state.getMaterial());
                block.updateTick(worldIn, pos, block.getDefaultState(), rand);
            }
        }
    }
}
