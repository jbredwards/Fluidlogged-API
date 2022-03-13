package git.jbredwards.fluidlogged_api.api.block;

import net.minecraft.block.state.IBlockState;
import net.minecraftforge.fluids.IFluidBlock;

import javax.annotation.Nonnull;

/**
 * Have your block implement this if it should be able to hold fluidloggable blocks.
 * @author jbred
 *
 */
public interface IFluidloggableFluid extends IFluidBlock
{
    boolean isFluidloggableFluid(@Nonnull IBlockState fluid, boolean checkLevel);
}
