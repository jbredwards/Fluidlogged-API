/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.actually_additions;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockDirectional;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.*;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * make actually additions' fluid placer/collector blocks FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginActuallyAdditions implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * doWork:
         * New code:
         * // use the IFluidBlock place & drain methods
         * @ASMOverwrite
         * private void doWork()
         * {
         *     Hooks.doWork(this, this.tank, this.isPlacer);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("doWork"),
            "doWork", "(Lnet/minecraft/tileentity/TileEntity;Lnet/minecraftforge/fluids/FluidTank;Z)V", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "de/ellpeck/actuallyadditions/mod/tile/TileEntityFluidCollector", "tank", "Lnet/minecraftforge/fluids/FluidTank;");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "de/ellpeck/actuallyadditions/mod/tile/TileEntityFluidCollector", "isPlacer", "Z");
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void doWork(@Nonnull final TileEntity tile, @Nonnull final FluidTank tank, final boolean isPlacer) {
            if(isPlacer && (tank.getFluid() == null || tank.getFluidAmount() < Fluid.BUCKET_VOLUME)) return;
            else if(!isPlacer && tank.getCapacity() - tank.getFluidAmount() < Fluid.BUCKET_VOLUME) return;
            @Nonnull final BlockPos offset = tile.getPos().offset(tile.getWorld().getBlockState(tile.getPos()).getValue(BlockDirectional.FACING));

            // place fluid
            if(isPlacer) {
                @Nullable final Block fluid = tank.getFluid().getFluid().getBlock();
                if(fluid instanceof IFluidBlock) {
                    @Nonnull final Chunk chunk = tile.getWorld().getChunk(offset);
                    @Nonnull final IBlockState neighbor = chunk.getBlockState(offset);
                    if(!FluidloggedUtils.isFluid(neighbor)) {
                        @Nonnull final FluidState neighborFluid = FluidState.getFromProvider(chunk, offset);
                        if(neighborFluid.isEmpty() && (neighbor.getBlock().isReplaceable(tile.getWorld(), offset)
                        || FluidloggedUtils.isStateFluidloggable(neighbor, tile.getWorld(), offset, FluidState.of(fluid)))) {
                            tank.drainInternal(((IFluidBlock)fluid).place(tile.getWorld(), offset, new FluidStack(tank.getFluid(), Fluid.BUCKET_VOLUME), true), true);
                        }
                    }
                }
            }

            // drain fluid
            else {
                @Nonnull final FluidState fluidState = FluidloggedUtils.getFluidState(tile.getWorld(), offset);
                if(fluidState.isValid()) tank.fillInternal(fluidState.getFluidBlock().drain(tile.getWorld(), offset, true), true);
            }
        }
    }
}
