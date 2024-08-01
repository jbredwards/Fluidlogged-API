/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidFlowHandler;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Random;

/**
 * fixes a bunch of liquid interactions while fluidlogged
 * @author jbred
 *
 */
public final class PluginBlockDynamicLiquid implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * updateTick:
         * New code:
         * // Rewrite flow logic to work while fluidlogged
         * @ASMGenerated
         * public void updateTick(World worldIn, BlockPos pos, IBlockState state, Random rand)
         * {
         *     Hooks.updateLiquidTick(worldIn, pos, state, rand);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180650_b" : "updateTick"),
            "updateLiquidTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V", generator -> {
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
        public static void updateLiquidTick(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final Random rand) {
            FluidFlowHandler.updateDynamic(world, pos, FluidState.of(state), rand);
        }
    }
}
