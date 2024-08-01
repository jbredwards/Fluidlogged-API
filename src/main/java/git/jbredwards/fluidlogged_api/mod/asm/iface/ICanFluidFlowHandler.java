/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.iface;

import net.minecraft.block.state.BlockStateBase;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.property.IExtendedBlockState;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Accessor implemented by {@link BlockStateBase} at runtime to allow for config-based canFluidFlow interactions.
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

    @Nonnull
    default ICanFluidFlowHandler negate() { return (world, pos, state, side) -> !canFluidFlow(world, pos, state, side); }
    boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull EnumFacing side);

    //Intended use only by config, implement IFluidloggable for custom modded canFluidFlow interactions.
    interface Accessor
    {
        @Nullable ICanFluidFlowHandler getCanFluidFlowOverride();
        @Nullable static ICanFluidFlowHandler getOverride(@Nonnull IBlockState state) {
            if(state instanceof IExtendedBlockState) state = ((IExtendedBlockState)state).getClean();
            return ((Accessor)state).getCanFluidFlowOverride();
        }

        void setCanFluidFlowOverride(@Nullable ICanFluidFlowHandler override);
        static void setOverride(@Nonnull Object state, @Nullable ICanFluidFlowHandler override) {
            if(state instanceof IExtendedBlockState) state = ((IExtendedBlockState)state).getClean();
            ((Accessor)state).setCanFluidFlowOverride(override);
        }
    }
}
