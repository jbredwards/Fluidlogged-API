/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.IWorldProvider;
import net.minecraft.block.BlockBush;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;

import javax.annotation.Nonnull;

/**
 * A basic implementation of an always waterlogged plant for mod devs to use.
 *
 * @since 1.8.0
 * @author jbred
 *
 */
public abstract class BlockWaterloggedPlant extends BlockBush implements IFluidloggable
{
    // used as a base to determine which fluids can support this plant
    @Nonnull protected Fluid parentFluid = FluidRegistry.WATER;

    protected BlockWaterloggedPlant(@Nonnull final Material materialIn) { this(materialIn, materialIn.getMaterialMapColor()); }
    protected BlockWaterloggedPlant(@Nonnull final Material materialIn, @Nonnull final MapColor mapColorIn) {
        super(materialIn, mapColorIn);
    }

    /**
     * This can only be placed in compatible fluid blocks.
     */
    @Override
    public boolean canPlaceBlockAt(@Nonnull final World worldIn, @Nonnull final BlockPos pos) {
        @Nonnull final FluidState fluidState = FluidloggedUtils.getFluidState(worldIn, pos);
        return !fluidState.isEmpty() && fluidState.isFluidloggable()
                && fluidState.getFluidBlockHandler().isFluidloggableFluid(fluidState)
                && isFluidloggable(getDefaultState(), worldIn, pos, fluidState)
                && super.canPlaceBlockAt(worldIn, pos);
    }

    /**
     * This can only be placed in compatible fluids
     */
    @Override
    public boolean isFluidValid(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final Fluid fluid) {
        return FluidloggedUtils.isCompatibleFluid(parentFluid, fluid);
    }

    /**
     * This can only be placed in source blocks or 1-block-tall fluids.
     */
    @Override
    public boolean isFluidloggable(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
        return fluidState.isEmpty() ? isFluidloggable(state, IWorldProvider.getWorld(world), pos) : isFluidValid(state, IWorldProvider.getWorld(world), pos, fluidState.getFluid())
           && (fluidState.isSource() || fluidState.getActualHeight(world, pos) >= 1 && FluidloggedUtils.canCreateSource(fluidState.getState(), IWorldProvider.getWorld(world), pos));
    }

    /**
     * Creates a new source block at this position (if the new fluid isn't one and if the new fluid can be turned into one).
     * This is the feature from Vanilla that lets you place kelp in waterfalls to turn them into source blocks.
     */
    @Nonnull
    @Override
    public EnumActionResult onFluidFill(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, @Nonnull final FluidState newFluid, final int blockFlags) {
        if(!newFluid.isSource()) {
            if(newFluid.getActualHeight(world, pos) < 1) {
                world.playEvent(Constants.WorldEvents.BREAK_BLOCK_EFFECTS, pos, getStateId(here));
                dropBlockAsItem(world, pos, here, 0);
                world.setBlockState(pos, newFluid.getState(), blockFlags);
                return EnumActionResult.SUCCESS;
            }

            else if(FluidloggedUtils.canCreateSource(newFluid.getState(), world, pos)
            && FluidloggedUtils.setFluidState(world, pos, here, newFluid.toSource(), false)) return EnumActionResult.SUCCESS;
        }

        return EnumActionResult.PASS;
    }

    /**
     * Breaks the plant here if the fluid is drained.
     */
    @Nonnull
    @Override
    public EnumActionResult onFluidDrain(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState here, final int blockFlags) {
        world.playEvent(Constants.WorldEvents.BREAK_BLOCK_EFFECTS, pos, getStateId(here));
        dropBlockAsItem(world, pos, here, 0);
        world.setBlockState(pos, Blocks.AIR.getDefaultState(), blockFlags);
        // skip updating the capability, as that was just handled through world#setBlockState
        return EnumActionResult.SUCCESS;
    }

    /**
     * A safety measure to keep this block's functionality in the event of a player recklessly disabling "applyDefaults".
     */
    @Override
    public boolean overrideApplyDefaultsSetting() { return true; }
}
