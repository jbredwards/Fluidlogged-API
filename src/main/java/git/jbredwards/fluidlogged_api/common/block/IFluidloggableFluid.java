package git.jbredwards.fluidlogged_api.common.block;

import net.minecraft.block.state.IBlockState;

import javax.annotation.Nonnull;

/**
 * Have your block implement this if it should be able to hold fluidloggable blocks.
 * Note that it's recommended to extend {@link net.minecraftforge.fluids.IFluidBlock}
 * when using this (BlockLiquid does already through asm)!
 * @author jbred
 *
 */
public interface IFluidloggableFluid
{
    boolean isFluidloggableFluid(@Nonnull IBlockState fluidState);
}
