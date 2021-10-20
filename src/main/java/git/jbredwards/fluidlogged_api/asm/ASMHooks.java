package git.jbredwards.fluidlogged_api.asm;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggableBase;
import net.minecraft.block.Block;
import org.apache.logging.log4j.Logger;

/**
 * class exists cause SpongeForge
 * NOTE THAT MOST OF THESE METHODS ARE MEANT TO ONLY BE USED IN CERTAIN CASES,
 * PRIOR TO INTEGRATING THEM TO YOUR OWN MOD, VIEW THE PLUGIN CLASS ASSOCIATED
 * @author jbred
 *
 */
@SuppressWarnings("unused")
public enum ASMHooks
{
    ;

    //FluidPlugin
    public static boolean updateIfNotFluidloggable(Block blockOld, Block blockNew) {
        return blockOld == blockNew && !(blockNew instanceof IFluidloggableBase);
    }

    //FluidPlugin
    public static void fluidBlockErrorSpamFix(Logger logger, String message, Block block, String fluidName, Block old) {
        if(!(block instanceof IFluidloggableBase)) logger.warn(message, block, fluidName, old);
    }
}
