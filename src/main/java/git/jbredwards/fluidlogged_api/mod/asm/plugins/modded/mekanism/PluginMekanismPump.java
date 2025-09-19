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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.mekanism;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import mekanism.api.Coord4D;
import mekanism.common.config.MekanismConfig;
import mekanism.common.tile.TileEntityElectricPump;
import mekanism.common.util.MekanismUtils;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.IFluidBlock;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * make mekanism's electric pump FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginMekanismPump implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // make mekanism's electric pump FluidState-sensitive
         * @ASMOverwrite
         * public boolean suck(boolean take)
         * {
         *     return Hooks.suck(this, take);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("suck"), "suck", "(Lmekanism/common/tile/TileEntityElectricPump;Z)Z", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ILOAD, 1);
        });

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean suck(@Nonnull final TileEntityElectricPump pump, final boolean notSimulated) {
            @Nonnull final Set<Coord4D> checked = new HashSet<>(Math.max((int)(pump.recurringNodes.size()/.75f) + 1, 16));
            @Nonnull final Coord4D coord = Coord4D.get(pump);

            if(suck(pump, notSimulated, coord, false, checked)) return true;
            for(@Nonnull final EnumFacing side : EnumFacing.VALUES) if(suck(pump, notSimulated, coord.offset(side), true, checked)) return true;

            @Nonnull final Coord4D[] shuffledCopy = pump.recurringNodes.toArray(new Coord4D[0]);
            Collections.shuffle(Arrays.asList(shuffledCopy));

            for(@Nonnull final Coord4D origin : shuffledCopy) {
                if(suck(pump, notSimulated, origin, false, checked)) return true;
                for(@Nonnull final EnumFacing side : EnumFacing.VALUES) {
                    @Nonnull final Coord4D offset = origin.offset(side);
                    if(coord.distanceTo(offset) <= MekanismConfig.current().general.maxPumpRange.val() && suck(pump, notSimulated, offset, true, checked)) return true;
                }

                pump.recurringNodes.remove(origin);
            }

            return false;
        }

        // helper
        public static boolean suck(@Nonnull final TileEntityElectricPump pump, final boolean notSimulated, @Nonnull final Coord4D coord, final boolean cache, @Nonnull final Set<Coord4D> checked) {
            if(checked.contains(coord)) return false;
            else checked.add(coord);

            @Nullable final FluidStack fluid = MekanismUtils.getFluid(pump.getWorld(), coord, pump.hasFilter());
            if(fluid != null && (pump.activeType == null || pump.activeType == fluid.getFluid())) {
                if(notSimulated) {
                    pump.activeType = fluid.getFluid();
                    pump.fluidTank.fill(fluid, true);

                    if(cache) pump.recurringNodes.add(coord);
                    @Nonnull final Block fluidBlock = fluid.getFluid().canBePlacedInWorld() ? fluid.getFluid().getBlock() : Blocks.WATER; // only heavy water falls back on normal water here
                    if(MekanismConfig.current().general.pumpWaterSources.val() || !FluidloggedUtils.canCreateSource(fluidBlock.getDefaultState(), pump.getWorld(), coord.getPos())) {
                        ((IFluidBlock)fluidBlock).drain(pump.getWorld(), coord.getPos(), true);
                    }
                }

                return true;
            }

            return false;
        }
    }
}
