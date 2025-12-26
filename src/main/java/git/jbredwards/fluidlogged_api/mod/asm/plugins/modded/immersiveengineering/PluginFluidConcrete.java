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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.immersiveengineering;

import blusunrize.immersiveengineering.api.IEProperties;
import blusunrize.immersiveengineering.common.IEContent;
import blusunrize.immersiveengineering.common.blocks.stone.BlockTypes_StoneDecoration;
import blusunrize.immersiveengineering.common.blocks.stone.BlockTypes_StoneDevices;
import blusunrize.immersiveengineering.common.util.IEPotions;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IFluidFlowListener;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.IFluidUpdateHelper;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.potion.PotionEffect;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.util.Constants;
import org.apache.commons.lang3.ArrayUtils;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fix issue#275
 * @author jbred
 *
 */
public final class PluginFluidConcrete implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.methods.removeIf(method -> method.name.equals(obfuscated ? "func_180650_b" : "updateTick"));
        classNode.interfaces.add(getAccessorClass());
        return false;
    }

    @SuppressWarnings("unused")
    public interface Accessor extends IFluidFlowListener, IFluidloggableFluid
    {
        IBlockState[] CONCRETE = {
                IEContent.blockStoneDevice.getStateFromMeta(BlockTypes_StoneDevices.CONCRETE_SHEET.getMeta()),
                IEContent.blockStoneDevice.getStateFromMeta(BlockTypes_StoneDevices.CONCRETE_QUARTER.getMeta()),
                IEContent.blockStoneDecorationSlabs.getStateFromMeta(BlockTypes_StoneDecoration.CONCRETE.getMeta()),
                IEContent.blockStoneDevice.getStateFromMeta(BlockTypes_StoneDevices.CONCRETE_THREEQUARTER.getMeta()),
                IEContent.blockStoneDecoration.getStateFromMeta(BlockTypes_StoneDecoration.CONCRETE.getMeta())
        };

        @Override
        default boolean preFluidUpdate(@Nonnull final IFluidUpdateHelper helper, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
            if(fluidState.getState().getValue(IEProperties.INT_16) >= Math.min(14, fluidState.getQuantaPerBlock() - fluidState.getLevel())) {
                if(helper.getBlockState(0, 0, 0).getBlock() != fluidState.getBlock()) return false; // Wait until not fluidlogged to turn into concrete.

                helper.getCache().getWorld().setBlockState(pos, CONCRETE[4 - (fluidState.getLevel() + 2 >> 2)]);
                helper.getCache().getWorld().getEntitiesWithinAABB(EntityLivingBase.class, new AxisAlignedBB(pos))
                        .forEach(living -> living.addPotionEffect(new PotionEffect(IEPotions.concreteFeet, Integer.MAX_VALUE)));

                return false;
            }

            helper.setFluid(0, 0, 0, FluidState.of(fluidState.getState().cycleProperty(IEProperties.INT_16)), false, 0, Constants.BlockFlags.NO_RERENDER);
            return true;
        }

        @Override
        default boolean delayCalculation(@Nonnull final IFluidUpdateHelper helper, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
            @Nonnull final FluidState[] flowTo = new FluidState[4];

            float blocks = 1;
            int total = fluidState.getLevel();
            for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
                if(helper.canFluidFlow(0, 0, 0, side) && helper.canFluidFlow(side.getXOffset(), 0, side.getZOffset(), side.getOpposite())) {
                    @Nonnull final FluidState neighbor = helper.getFluidState(side.getXOffset(), 0, side.getZOffset());
                    if(FluidloggedUtils.isCompatibleFluid(fluidState, neighbor)) {
                        flowTo[side.getHorizontalIndex()] = neighbor;
                        blocks++;
                        total += neighbor.getLevel();
                    }
                }
            }

            final int newEvenQuanta = MathHelper.ceil(total/blocks);
            for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
                @Nullable final FluidState neighbor = flowTo[side.getHorizontalIndex()];
                if(neighbor != null) helper.setFluid(side.getXOffset(), 0, side.getZOffset(), neighbor.withLevel(newEvenQuanta), false, 0);
            }

            return true;
        }

        @Override
        default void postFluidUpdate(@Nonnull final IFluidUpdateHelper helper, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState, final boolean hasFlown) {
            if(!hasFlown) helper.getCache().getWorld().scheduleUpdate(pos, fluidState.getBlock(), fluidState.getBlock().tickRate(helper.getCache().getWorld()));
        }

        @Override
        default boolean isStateFluidloggable(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
            return !ArrayUtils.contains(CONCRETE, state) && IFluidloggableFluid.super.isStateFluidloggable(state, world, pos, fluidState);
        }
    }
}
