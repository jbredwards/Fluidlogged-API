package git.jbredwards.fluidlogged_api.common.block;

/**
 * Have your IFluidBlock implement this if it should be able to hold fluidloggable blocks.
 * Note that it's recommended to extend {@link net.minecraftforge.fluids.IFluidBlock}
 * when using this (BlockLiquid does already through asm)!
 * @author jbred
 *
 */
public interface IFluidloggableFluid
{
    boolean isFluidloggableFluid();
}
