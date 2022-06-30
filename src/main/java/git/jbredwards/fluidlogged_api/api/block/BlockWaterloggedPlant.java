package git.jbredwards.fluidlogged_api.api.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.BlockBush;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;

import javax.annotation.Nonnull;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;

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
        final FluidState fluidState = getFluidState(worldIn, pos);
        return !fluidState.isEmpty()
                && isFluidValid(getDefaultState(), worldIn, pos, fluidState.getFluid())
                && isFluidloggableFluid(fluidState.getState(), worldIn, pos)
                && super.canPlaceBlockAt(worldIn, pos);
    }

    /**
     * Ensures that only compatible fluids can be placed inside this
     */
    @Override
    public boolean isFluidValid(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Fluid fluid) {
        return isCompatibleFluid(parentFluid, fluid);
    }

    /**
     * Breaks the plant here if the fluid is drained
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
}
