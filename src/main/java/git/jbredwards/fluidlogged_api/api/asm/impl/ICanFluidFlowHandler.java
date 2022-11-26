package git.jbredwards.fluidlogged_api.api.asm.impl;

import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Accessor implemented by {@link net.minecraft.block.Block Block} at runtime to allow for config-based canFluidFlow interactions.
 * @author jbred
 *
 */
//TODO crafttweaker/groovyscript support maybe?
@FunctionalInterface
public interface ICanFluidFlowHandler
{
    @Nonnull
    ICanFluidFlowHandler
            ALWAYS_FLOW = (world, pos, state, side) -> true,
            NEVER_FLOW = (world, pos, state, side) -> false,
            DEPRECATED_CHECK = (world, pos, state, side) -> !state.isSideSolid(world, pos, side);

    boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull EnumFacing side);

    //Intended use only be config, implement IFluidloggable for custom modded canFluidFlow interactions.
    interface Accessor
    {
        @Nullable ICanFluidFlowHandler getCanFluidFlowOverride();
        @Nullable static ICanFluidFlowHandler getOverride(@Nonnull Block block) {
            return ((Accessor)block).getCanFluidFlowOverride();
        }

        void setCanFluidFlowOverride(@Nullable ICanFluidFlowHandler override);
        static void setOverride(@Nonnull Block block, @Nullable ICanFluidFlowHandler override) {
            ((Accessor)block).setCanFluidFlowOverride(override);
        }
    }
}
