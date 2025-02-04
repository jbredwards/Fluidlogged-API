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
 * A basic implementation of an always waterlogged plant for mod devs to use
 * @author jbred
 *
 */
public abstract class BlockWaterloggedPlant extends BlockBush implements IFluidloggable
{
    //used as a base to determine which fluids can support this plant
    @Nonnull protected Fluid parentFluid = FluidRegistry.WATER;

    protected BlockWaterloggedPlant(@Nonnull Material materialIn) {
        this(materialIn, materialIn.getMaterialMapColor());
    }

    protected BlockWaterloggedPlant(@Nonnull Material materialIn, @Nonnull MapColor mapColorIn) {
        super(materialIn, mapColorIn);
    }

    /**
     * This can only be placed in compatible fluid blocks
     */
    @Override
    public boolean canPlaceBlockAt(@Nonnull World worldIn, @Nonnull BlockPos pos) {
        final FluidState fluidState = FluidloggedUtils.getFluidState(worldIn, pos);
        return !fluidState.isEmpty() && fluidState.isFluidloggable()
                && fluidState.getFluidBlockHandler().isFluidloggableFluid(fluidState)
                && isFluidloggable(getDefaultState(), worldIn, pos, fluidState)
                && super.canPlaceBlockAt(worldIn, pos);
    }

    /**
     * Ensures that only compatible fluids can be placed inside this
     */
    @Override
    public boolean isFluidValid(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Fluid fluid) {
        return FluidloggedUtils.isCompatibleFluid(parentFluid, fluid);
    }

    /**
     *
     * @param state
     * @param world
     * @param pos
     * @param fluidState
     * @return
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     */
    @Override
    public boolean isFluidloggable(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull FluidState fluidState) {
        return fluidState.isEmpty() ? isFluidloggable(state, IWorldProvider.getWorld(world), pos) : isFluidValid(state, IWorldProvider.getWorld(world), pos, fluidState.getFluid())
           && (fluidState.isSource() || fluidState.getActualHeight(world, pos) == 1 && FluidloggedUtils.canCreateSource(fluidState.getState(), IWorldProvider.getWorld(world), pos));
    }

    /**
     * Creates a new source block at this position, if the new fluid isn't one, and if the new fluid can be turned into one.
     */
    @Nonnull
    @Override
    public EnumActionResult onFluidFill(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull FluidState newFluid, int blockFlags) {
        return !newFluid.isSource() && newFluid.getActualHeight(world, pos) == 1
                && FluidloggedUtils.canCreateSource(newFluid.getState(), world, pos)
                && FluidloggedUtils.setFluidState(world, pos, here, newFluid.toSource(), false)
                ? EnumActionResult.SUCCESS : EnumActionResult.PASS;
    }

    /**
     * Breaks the plant here if the fluid is drained.
     */
    @Nonnull
    @Override
    public EnumActionResult onFluidDrain(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, int blockFlags) {
        world.playEvent(Constants.WorldEvents.BREAK_BLOCK_EFFECTS, pos, getStateId(here));
        dropBlockAsItem(world, pos, here, 0);
        world.setBlockState(pos, Blocks.AIR.getDefaultState(), blockFlags);
        //skip updating the capability, as that was just handled through world#setBlockState
        return EnumActionResult.SUCCESS;
    }

    @Override
    public boolean overrideApplyDefaultsSetting() { return true; }
}
