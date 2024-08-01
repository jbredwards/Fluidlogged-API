/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

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
        /*
         * getCanBlockBurn:
         * New code:
         * //check if the FluidState here is air or flammable
         * private boolean getCanBlockBurn(World worldIn, BlockPos pos)
         * {
         *     return Hooks.getCanBlockBurn(worldIn, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176368_m" : "getCanBlockBurn"),
            "getCanBlockBurn", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean getCanBlockBurn(@Nonnull final World world, @Nonnull final BlockPos pos) {
            if(world.isOutsideBuildHeight(pos) || !world.isBlockLoaded(pos)) return false;

            @Nonnull final Chunk chunk = world.getChunk(pos);
            if(!chunk.getBlockState(pos).getMaterial().getCanBurn()) return false;

            @Nonnull final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
            return fluidState == FluidState.EMPTY || fluidState.getMaterial().getCanBurn();
        }

        public static void updateLiquid(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
            @Nonnull final FluidState dynState = FluidState.of(BlockLiquid.getFlowingBlock(state.getMaterial())).withLevel(state.getValue(BlockLiquid.LEVEL));

            if(FluidState.get(worldIn, pos).isEmpty()) worldIn.setBlockState(pos, dynState.getState(), Constants.BlockFlags.SEND_TO_CLIENTS);
            else FluidloggedUtils.setFluidState(worldIn, pos, null, dynState, false, Constants.BlockFlags.SEND_TO_CLIENTS);

            worldIn.scheduleUpdate(pos, dynState.getBlock(), dynState.getBlock().tickRate(worldIn));
        }
    }
}
