package git.jbredwards.fluidlogged_api.api.event;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;

/**
 * fired when trying to fluidlog/unfluidlog a block
 * if cancelled, the block will not become fluidlogged
 * result default = default code gets ran; result allow = block was fluidlogged; result deny = block was not fluidlogged
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

    public boolean doesVaporize() {
        return checkVaporize && !fluidState.isEmpty() && world.provider.doesWaterVaporize()
                && fluidState.getFluid().doesVaporize(getFluidStack());
    }

    //throws an exception if fluidState is empty
    @Nonnull
    public FluidStack getFluidStack() { return new FluidStack(fluidState.getFluid(), Fluid.BUCKET_VOLUME); }
}
