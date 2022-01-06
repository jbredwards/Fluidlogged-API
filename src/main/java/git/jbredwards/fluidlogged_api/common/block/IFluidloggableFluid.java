package git.jbredwards.fluidlogged_api.common.block;

import net.minecraft.block.Block;
import net.minecraftforge.fluids.IFluidBlock;

/**
 * Have your IFluidBlock implement this if it should be able to hold fluidloggable blocks.
 * Note that you'll likely have to change a lot of your block's code to get this implementation working!
 * @author jbred
 *
 */
public interface IFluidloggableFluid extends IFluidBlock
{
    @SuppressWarnings("deprecation")
    default boolean isFluidloggableFluid() { return this instanceof Block && !((Block)this).hasTileEntity(); }
}
