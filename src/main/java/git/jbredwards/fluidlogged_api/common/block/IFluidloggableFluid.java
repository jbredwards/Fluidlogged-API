package git.jbredwards.fluidlogged_api.common.block;

import net.minecraft.block.Block;
import net.minecraft.init.Blocks;

/**
 * Have your IFluidBlock implement this if it should be able to hold fluidloggable blocks.
 * Note that it's recommended to extend {@link net.minecraftforge.fluids.IFluidBlock}
 * when using this (BlockLiquid does already through asm)!
 * @author jbred
 *
 */
public interface IFluidloggableFluid
{
    default boolean isFluidFluidloggable() {
        if(!(this instanceof Block)) return false;
        final Block block = (Block)this;

        //most modded BlockLiquid instances involve blocks that typically shouldn't be fluidloggable fluids (like coral)
        //if the block should be fluidloggable though, implement this as an Optional interface
        return block == Blocks.FLOWING_LAVA
                || block == Blocks.LAVA
                || block == Blocks.FLOWING_WATER
                || block == Blocks.WATER;
    }
}
