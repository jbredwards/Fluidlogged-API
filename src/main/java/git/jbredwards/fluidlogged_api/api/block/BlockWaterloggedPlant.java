package git.jbredwards.fluidlogged_api.api.block;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
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

/**
 * A basic implementation of an always waterlogged plant for mod devs to use
 * @author jbred
 *
 */
public abstract class BlockWaterloggedPlant extends BlockBush implements IFluidloggable
{
    protected BlockWaterloggedPlant(@Nonnull Material materialIn) {
        this(materialIn, materialIn.getMaterialMapColor());
    }

    protected BlockWaterloggedPlant(@Nonnull Material materialIn, @Nonnull MapColor mapColorIn) {
        super(materialIn, mapColorIn);
    }

    /**
     * This can only be placed in water-like blocks
     */
    @Override
    public boolean canPlaceBlockAt(@Nonnull World worldIn, @Nonnull BlockPos pos) {
        return isFluidValid(getDefaultState(), FluidloggedUtils.getFluidState(worldIn, pos).getFluid());
    }

    /**
     * Ensures that only water-like fluids can be placed inside this
     */
    @Override
    public boolean isFluidValid(@Nonnull IBlockState state, @Nonnull Fluid fluid) {
        return FluidloggedUtils.isCompatibleFluid(FluidRegistry.WATER, fluid);
    }

    /**
     * Remove this if the fluid here is drained
     */
    @Nonnull
    @Override
    public EnumActionResult onFluidDrain(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, int blockFlags) {
        //break this
        world.playEvent(Constants.WorldEvents.BREAK_BLOCK_EFFECTS, pos, getStateId(here));
        dropBlockAsItem(world, pos, here, 0);
        world.setBlockState(pos, Blocks.AIR.getDefaultState(), blockFlags);
        //skip updating the capability, as that was just handled through world#setBlockState
        return EnumActionResult.SUCCESS;
    }
}
