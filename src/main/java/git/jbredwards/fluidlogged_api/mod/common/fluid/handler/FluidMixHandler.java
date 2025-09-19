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

package git.jbredwards.fluidlogged_api.mod.common.fluid.handler;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;

/**
 *
 * @author jbred
 *
 */
public final class FluidMixHandler
{
    /**
     * Checks for mixing with neighbors. This method does not change the fluid here, and instead replaces the neighbor blocks.
     */
    public static boolean tryMixAtNeighbors(@Nonnull final FluidState source, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final Collection<MixCondition> mixConditions, @Nullable final BiConsumer<World, BlockPos> mixEffects) {
        @Nonnull final FluidCache cache = new FluidCache(world, pos, 1, 1);

        boolean ret = false;
        for(@Nonnull final EnumFacing side : EnumFacing.VALUES) {
            if(side != source.getUpDensityFace() && (!FluidloggedAPIConfig.fixBadFluidMixing || FluidloggedUtils.canFluidFlow(cache, pos, cache.getBlockState(pos), side))) {
                @Nonnull final BlockPos offset = pos.offset(side);
                if(!FluidloggedUtils.isCompatibleFluid(source, cache.getFluidOrReal(offset))) { // never mix with itself
                    @Nonnull final Optional<IBlockState> result = mixConditions.stream().map(condition -> condition.getMixResult(source, cache, offset, pos, side)).filter(Objects::nonNull).findFirst();
                    if(result.isPresent() && (ret = world.setBlockState(offset, ForgeEventFactory.fireFluidPlaceBlockEvent(world, offset, pos, result.get()))) && mixEffects != null) mixEffects.accept(world, offset);
                }
            }
        }

        return ret;
    }

    /**
     * @return A {@link MixCondition} that checks that the block here is replaceable, and that the fluid can flow into it. This method is intended for fluid mixing with other fluids.
     */
    @Nonnull
    public static MixCondition forFluid(@Nonnull final MixCondition mixCondition) {
        return (source, access, pos, sourcePos, side) -> (!FluidloggedAPIConfig.fixBadFluidMixing || FluidloggedUtils.canFluidFlow(access, pos, access.getBlockState(pos), side.getOpposite()))
                && access.getBlockState(pos).getBlock().isReplaceable(access, pos) ? mixCondition.getMixResult(source, access, pos, sourcePos, side) : null;
    }

    /**
     * Checks if the position (and its contents: ie. a block or fluid) is valid for mixing.
     */
    @FunctionalInterface
    public interface MixCondition
    {
        @Nullable
        IBlockState getMixResult(@Nonnull final FluidState source, @Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final BlockPos sourcePos, @Nonnull final EnumFacing side);
    }
}
