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
import git.jbredwards.fluidlogged_api.api.world.IWorldProvider;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigFluidBox;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IWaterHeight;
import git.jbredwards.fluidlogged_api.mod.asm.transformers.TransformerMethodRedirects;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.BlockStateContainer;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.IFluidBlock;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 *
 * @author jbred
 *
 */
public final class FluidCollisionHandler
{
    @Nonnull
    @SideOnly(Side.CLIENT)
    public static IBlockState getStateAtViewpoint(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final Vec3d viewpoint) {
        @Nonnull final Entity viewpointEntity = Objects.requireNonNull(Minecraft.getMinecraft().getRenderViewEntity());
        if(isWithinFluid(world, pos, viewpoint.x, viewpoint.y, viewpoint.z, state, viewpointEntity.getEntityBoundingBox(), viewpointEntity, false, true)) return state;
        // return the other block here if the player isn't within the fluid
        @Nonnull final IBlockState here = world.getBlockState(pos);
        return here == state ? BlockStateContainer.AIR_BLOCK_STATE : here.getBlock().getStateAtViewpoint(here, world, pos, viewpoint);
    }

    @Nullable
    public static Boolean isAABBInsideMaterial(@Nonnull final Material material, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final AxisAlignedBB bb, @Nonnull final Material materialToTest) {
        return material != materialToTest ? null : isAABBInsideLiquid(world, pos, bb.shrink(0.001), null, TransformerMethodRedirects.Hooks.currentEntity, false);
    }

    @Nonnull
    public static Boolean isAABBInsideLiquid(@Nonnull final IBlockAccess worldIn, @Nonnull final BlockPos pos, @Nonnull final AxisAlignedBB bb) {
        return isAABBInsideLiquid(worldIn, pos, bb.shrink(0.001), null, TransformerMethodRedirects.Hooks.currentEntity, false);
    }

    @Nullable
    public static Boolean isEntityInsideMaterial(@Nonnull final IBlockAccess worldIn, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final Entity entity, final double yToTest, @Nonnull final Material materialIn, final boolean testingHead) {
        if(materialIn != state.getMaterial() || !(state instanceof IExtendedBlockState)) return null;
        else return testingHead ? Boolean.valueOf(isWithinFluid(worldIn, pos, entity.posX, yToTest, entity.posZ, state, entity.getEntityBoundingBox().shrink(0.001), entity, true, false)) : isAABBInsideLiquid(worldIn, pos, entity.getEntityBoundingBox().shrink(0.001), state, entity, true);
    }

    // ==========================
    // INTERNAL UTILITY FUNCTIONS
    // ==========================

    @Nonnull
    public static final ThreadLocal<IWaterHeight> cacheHeight = new ThreadLocal<>();

    @Nonnull
    static Boolean isAABBInsideLiquid(@Nonnull final IBlockAccess worldIn, @Nonnull final BlockPos pos, @Nonnull final AxisAlignedBB bb, @Nullable final IBlockState stateIn, @Nullable final Object entity, final boolean checkCache) {
        if(bb.minX > pos.getX() + 1 || bb.maxX < pos.getX() || bb.minZ > pos.getZ() + 1 || bb.maxZ < pos.getZ()) return Boolean.FALSE; // don't collide with fluid if BB doesn't horizontally intersect
        else if(!FluidloggedAPIConfig.fancyFluidEntityCollision.test(bb, entity)
                || bb.minX < pos.getX() && bb.maxX > pos.getZ() + 1 && bb.minZ < pos.getZ() && bb.maxZ > pos.getZ() + 1
                || !IWorldProvider.getWorld(worldIn).isAreaLoaded(pos.add(-1, -1, -1), pos.add(1, 1, 1))) {
            @Nonnull final IBlockAccess world = worldIn instanceof World ? new FluidCache(worldIn, pos, 0, 1) : worldIn;
            return isYWithinFluidEstimate(world, pos, bb.minY, bb.maxY, stateIn != null ? stateIn : FluidloggedUtils.getFluidOrReal(world, pos), checkCache, false);
        }

        @Nonnull final IBlockAccess access = worldIn instanceof World ? new FluidCache(worldIn, pos, 1, 1) : worldIn;
        @Nonnull final IBlockState state = stateIn != null ? stateIn : FluidloggedUtils.getFluidOrReal(access, pos);
        @Nonnull final IBlockState extendedState = state.getBlock().getExtendedState(state, access, pos);

        return !(extendedState instanceof IExtendedBlockState) ? isYWithinFluidEstimate(access, pos, bb.minY, bb.maxY, state, checkCache, false)
                :  isPointWithinFluid(pos, bb.minX, bb.minY, bb.maxY, bb.minZ, (IExtendedBlockState)extendedState, checkCache, false)
                || isPointWithinFluid(pos, bb.minX, bb.minY, bb.maxY, bb.maxZ, (IExtendedBlockState)extendedState, checkCache, false)
                || isPointWithinFluid(pos, bb.maxX, bb.minY, bb.maxY, bb.minZ, (IExtendedBlockState)extendedState, checkCache, false)
                || isPointWithinFluid(pos, bb.maxX, bb.minY, bb.maxY, bb.maxZ, (IExtendedBlockState)extendedState, checkCache, false);
    }

    @SuppressWarnings("UnnecessaryLocalVariable")
    static boolean isPointWithinFluid(@Nonnull final BlockPos pos, final double xIn, final double minY, final double maxY, final double zIn, @Nonnull final IExtendedBlockState state, final boolean checkCache, final boolean visualOnly) {
        @Nonnull final float[][] corners = new float[2][2];
        corners[0][0] = state.getValue(BlockFluidBase.LEVEL_CORNERS[0]);
        corners[0][1] = state.getValue(BlockFluidBase.LEVEL_CORNERS[1]);
        corners[1][1] = state.getValue(BlockFluidBase.LEVEL_CORNERS[2]);
        corners[1][0] = state.getValue(BlockFluidBase.LEVEL_CORNERS[3]);

        // unit position of the point, relative to the fluid pos
        final double x = MathHelper.clamp(xIn, pos.getX(), pos.getX() + 1) - pos.getX();
        final double z = MathHelper.clamp(zIn, pos.getZ(), pos.getZ() + 1) - pos.getZ();

        // gets the exact slope height of the fluid at the (x, z) point
        final double x_weight_0 = 1 - x, x_weight_1 = x;
        final double z_weight_0 = 1 - z, z_weight_1 = z;
        final double fluidHeightAtPoint
                = corners[0][0] * x_weight_0 * z_weight_0
                + corners[0][1] * x_weight_0 * z_weight_1
                + corners[1][1] * x_weight_1 * z_weight_1
                + corners[1][0] * x_weight_1 * z_weight_0;
        return isYWithinFluid(FluidloggedUtils.getFluidFromState(state), pos, minY, maxY, applyQolOffset(fluidHeightAtPoint, visualOnly), checkCache);
    }

    static boolean isWithinFluid(@Nonnull final IBlockAccess worldIn, @Nonnull final BlockPos pos, final double x, final double y, final double z, @Nullable final IBlockState stateIn, @Nonnull final AxisAlignedBB bb, @Nullable final Entity entity, final boolean checkCache, final boolean visualOnly) {
        if(FluidloggedAPIConfig.fancyFluidEntityCollision.test(bb, entity) && IWorldProvider.getWorld(worldIn).isAreaLoaded(pos.add(-1, -1, -1), pos.add(1, 1, 1))) {
            @Nonnull final IBlockAccess access = worldIn instanceof World ? new FluidCache(worldIn, pos, 1, 1) : worldIn;
            @Nonnull final IBlockState state = stateIn != null ? stateIn : FluidloggedUtils.getFluidOrReal(access, pos);
            @Nonnull final IBlockState extendedState = state.getBlock().getExtendedState(state, access, pos);

            return extendedState instanceof IExtendedBlockState ? isPointWithinFluid(pos, x, y, y, z, (IExtendedBlockState)extendedState, checkCache, visualOnly) : isYWithinFluidEstimate(access, pos, y, y, state, checkCache, visualOnly);
        }

        else {
            @Nonnull final IBlockAccess access = worldIn instanceof World ? new FluidCache(worldIn, pos, 0, 1) : worldIn;
            return isYWithinFluidEstimate(access, pos, y, y, stateIn != null ? stateIn : FluidloggedUtils.getFluidOrReal(access, pos), checkCache, visualOnly);
        }
    }

    static boolean isYWithinFluidEstimate(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, final double minY, final double maxY, @Nonnull final IBlockState state, final boolean checkCache, final boolean visualOnly) {
        return isYWithinFluid(FluidloggedUtils.getFluidFromState(state), pos, minY, maxY, applyQolOffset(getFilledPercentage(FluidState.of(state), world, pos), visualOnly), checkCache);
    }

    static boolean isYWithinFluid(@Nullable final Fluid fluid, @Nonnull final BlockPos pos, final double minY, final double maxY, final double fluidHeight, final boolean checkCache) {
        final boolean gas = fluid != null && fluid.isLighterThanAir();
        final boolean isWithin = gas ? maxY > pos.getY() + 1 - fluidHeight && minY <= pos.getY() + 1 : minY <= pos.getY() + fluidHeight && maxY > pos.getY();

        if(!isWithin) return false;
        else if(checkCache && FluidloggedAPIConfig.ignoreLowFluidCollision) {
            @Nullable final IWaterHeight waterHeight = cacheHeight.get();
            if(waterHeight != null) {
                @Nullable final IConfigFluidBox.HeightBox box = waterHeight.getBox();
                final double height = Math.min(pos.getY() + fluidHeight - minY, 1);

                if(box == null) waterHeight.setBox(new IConfigFluidBox.HeightBox(gas ? 1 - height : 0, gas ? 1 : height));
                else if(box.min != 0 || box.max != 1) waterHeight.setBox(new IConfigFluidBox.HeightBox(gas ? Math.min(box.min, 1 - height) : 0, gas ? 1 : Math.max(box.max, height)));
            }
        }

        return true;
    }

    public static double applyQolOffset(final double fluidHeight) { return applyQolOffset(fluidHeight, false); }
    public static double applyQolOffset(final double fluidHeight, final boolean visualOnly) {
        if(fluidHeight >= 1-1E-2) return 1; // handle possible floating point errors (fixes #299)
        else if(visualOnly) return fluidHeight + 0.055; // helps prevent the player camera from clipping into water, without applying correct fog
        final double maxHeight = 0.875; // move the level to check down slightly, so things like lava next to soul sand don't light players on fire
        return Math.min(fluidHeight, maxHeight);
    }

    public static float getFilledPercentage(@Nonnull final FluidState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
        if(state.getBlock() instanceof BlockLiquid) return BlockLiquid.getBlockLiquidHeight(state.getState(), world, pos);
        final float remaining = state.getBlock() instanceof BlockFluidBase ? ((BlockFluidBase)state.getBlock()).getFilledPercentage(world, pos) // don't use World if the block is a BlockFluidBase
                : state.isValid() ? state.getFluidBlock().getFilledPercentage(IWorldProvider.getWorld(world), pos) : 1; // should never pass (most IFluidBlock blocks should be BlockFluidBase)

        // fixes a general inaccuracy with modded fluids (this especially comes up in other mods like Biomes O'Plenty)
        final float filled = remaining >= 1-1E-2 ? remaining : remaining * state.getQuantaFraction();
        return filled < 0 ? 1 - filled : filled; // ensure this is positive (gaseous fluids measure top-down)
    }

    public static boolean isYWithinMaterialEstimate(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, final double minY, final double maxY, @Nonnull final Material target) {
        @Nonnull final IBlockAccess access = world instanceof World ? new FluidCache(world, pos, 0, 1) : world;
        @Nonnull final IBlockState fluidState = FluidloggedUtils.getFluidOrReal(access, pos);

        if(!(fluidState.getBlock() instanceof IFluidBlock)) {
            @Nonnull final AxisAlignedBB bb = new AxisAlignedBB(pos.getX(), minY, pos.getZ(), pos.getX() + 1, maxY, pos.getZ() + 1);
            return isAABBInsideMaterial(fluidState, world, pos, bb, target);
        }

        return fluidState.getMaterial() == target && isYWithinFluidEstimate(access, pos, minY, maxY, fluidState, false, false);
    }

    public static boolean isAABBInsideMaterial(@Nonnull final IBlockState state, @Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final AxisAlignedBB bb, @Nonnull final Material material) {
        @Nullable final Boolean ret = state.getBlock().isAABBInsideMaterial(IWorldProvider.getWorld(access), pos, bb, material);
        return ret == null ? state.getMaterial() == material : ret;
    }
}
