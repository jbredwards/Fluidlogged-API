package git.jbredwards.fluidlogged_api.api.event;

import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Fired through {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils#isStateFluidloggable(IBlockState, World, BlockPos, Fluid) FluidloggedUtils.isStateFluidloggable()}.
 * For modded blocks it's recommended to implement {@link git.jbredwards.fluidlogged_api.api.block.IFluidloggable IFluidloggable} instead of using this event,
 * it also provides more functionality.
 * @author jbred
 *
 */
@Cancelable
@Event.HasResult
public class FluidloggableEvent extends Event
{
    @Nonnull public final IBlockState state;
    @Nonnull public final World world;
    @Nonnull public final BlockPos pos;
    @Nullable public final Fluid fluid;

    public FluidloggableEvent(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nullable Fluid fluid) {
        this.state = state;
        this.world = world;
        this.pos = pos;
        this.fluid = fluid;
    }
}
