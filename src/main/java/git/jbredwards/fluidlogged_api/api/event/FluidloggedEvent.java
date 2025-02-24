/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.event;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;

/**
 * This event is fired on the {@link net.minecraftforge.common.MinecraftForge#EVENT_BUS} when fluidlogging/un-fluidlogging a block.<br>
 * <br>
 * This event is {@link Cancelable cancelable}.<br>
 * If this event is canceled, the block will not become fluidlogged.<br>
 * <br>
 * This event has a {@link HasResult result}:
 * <li>{@link Result#ALLOW} means that the state was fluidlogged.</li>
 * <li>{@link Result#DEFAULT} means that the default code for fluidlogging the state will run.</li>
 * <li>{@link Result#DENY} means that the state was not fluidlogged.</li>
 *
 * @since 1.7.0
 * @author jbred
 *
 */
@Cancelable
@Event.HasResult
public class FluidloggedEvent extends Event
{
    @Nonnull public final World world;
    @Nonnull public final Chunk chunk;
    @Nonnull public final BlockPos pos;
    @Nonnull public final IBlockState here;
    @Nonnull public FluidState fluidState;
    public boolean checkVaporize;
    public int blockFlags;

    public FluidloggedEvent(@Nonnull World world, @Nonnull Chunk chunk, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull FluidState fluidState, boolean checkVaporize, int blockFlags) {
        this.world = world;
        this.chunk = chunk;
        this.pos = pos;
        this.here = here;
        this.fluidState = fluidState;
        this.checkVaporize = checkVaporize;
        this.blockFlags = blockFlags;
    }

    /**
     * @return True if fluidState will vaporize.
     * @since 1.9.0
     * @author jbred
     */
    public boolean doesVaporize() {
        return checkVaporize && !fluidState.isEmpty() && world.provider.doesWaterVaporize() && fluidState.getFluid().doesVaporize(getFluidStack());
    }

    /**
     * {@link FluidState#isValid() fluidState.isValid()} should typically be checked at least once before this method.
     *
     * @return A new {@link FluidStack} containing this fluidState's fluid.
     * The returned {@link FluidStack} will have a size of 0 if fluidState is not a source block,
     * or a size based on its level and quanta if it's a {@link net.minecraftforge.fluids.BlockFluidFinite}.
     * @throws UnsupportedOperationException If fluidState is not valid.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    public FluidStack getFluidStack() { return fluidState.createFluidStack(); }
}
