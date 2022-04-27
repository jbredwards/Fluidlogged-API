package git.jbredwards.fluidlogged_api.api.block;

import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.IFluidBlock;

import javax.annotation.Nonnull;

/**
 * Have your fluid block implement this if it should be able to hold fluidloggable blocks.
 * @author jbred
 *
 */
public interface IFluidloggableFluid extends IFluidBlock
{
    /**
     * Used when the fluid is in the world
     */
    default boolean isFluidloggableFluid(@Nonnull IBlockState fluid, @Nonnull World world, @Nonnull BlockPos pos) {
        return isFluidloggableFluid();
    }

    /**
     * Used when the fluid isn't in the world (bucket fluidlogging for example)
     */
    boolean isFluidloggableFluid();
}
