/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants.WorldEvents;
import org.apache.commons.lang3.tuple.Pair;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.LinkedList;
import java.util.Queue;

/**
 * fixes drain interactions across all modded fluids & FluidStates
 * @author jbred
 *
 */
public final class PluginBlockSponge implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * absorb:
         * New code:
         * //absorb FluidStates
         * private boolean absorb(World worldIn, BlockPos pos)
         * {
         *     return Hooks.absorb(worldIn, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176312_d" : "absorb"),
            "absorb", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean absorb(@Nonnull World world, @Nonnull BlockPos origin) {
            final Queue<Pair<BlockPos, Integer>> queue = new LinkedList<>();
            queue.add(Pair.of(origin, 0));
            int absorbed = 0;

            while(!queue.isEmpty()) {
                final Pair<BlockPos, Integer> entry = queue.poll();
                final BlockPos pos = entry.getKey();
                final int distance = entry.getValue();

                for(EnumFacing facing : EnumFacing.values()) {
                    final BlockPos offset = pos.offset(facing);
                    final FluidState fluidState = FluidloggedUtils.getFluidState(world, offset);

                    if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER && drain(world, offset, fluidState, 2)) {
                        if(distance < 6) queue.add(Pair.of(offset, distance + 1));
                        absorbed++;
                    }
                }
            }

            return absorbed > 0;
        }

        // helper
        public static boolean drain(@Nonnull final World world, @Nonnull final BlockPos pos, final int flags, @Nonnull final IBlockState fluidState) {
            return drain(world, pos, FluidState.of(fluidState), flags);
        }

        // helper
        public static boolean drain(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState, final int flags) {
            // don't drain bad fluid blocks (looking at you BOP kelp)
            if(fluidState.isValid()) {
                fluidState.getFluidBlock().drain(world, pos, true);
                return true;
            }
            // drain bad fluid blocks
            else if(world.setBlockState(pos, Blocks.AIR.getDefaultState(), flags | 32)) {
                world.playEvent(WorldEvents.BREAK_BLOCK_EFFECTS, pos, Block.getStateId(fluidState.getState()));
                if(fluidState.getBlock().getHarvestTool(fluidState.getState()) == null)
                    fluidState.getBlock().dropBlockAsItem(world, pos, fluidState.getState(), 0);
                return true;
            }
            // should never pass
            return false;
        }
    }
}
