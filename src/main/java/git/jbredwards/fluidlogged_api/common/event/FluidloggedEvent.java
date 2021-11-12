package git.jbredwards.fluidlogged_api.common.event;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public abstract class FluidloggedEvent extends Event
{
    //fired when trying to fluidlog/unfluidlog a block
    //if cancelled, the block will not become fluidlogged
    //result default = default code gets ran; result allow = block was fluidlogged; result deny = block was not fluidlogged
    @Cancelable
    @HasResult
    public static class Fluidlog extends FluidloggedEvent
    {
        @Nonnull  public final World world;
        @Nonnull  public final BlockPos pos;
        @Nonnull  public final IBlockState state;
        @Nonnull public FluidState fluidState;
        public int flags;
        public boolean checkVaporize;

        public Fluidlog(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull FluidState fluidState, boolean checkVaporize, int flags) {
            this.world = world;
            this.pos = pos;
            this.state = state;
            this.fluidState = fluidState;
            this.checkVaporize = checkVaporize;
            this.flags = flags;
        }

        @Nonnull
        public static Pair<Fluidlog, Result> post(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull FluidState fluidState, boolean checkVaporize, int flags) {
            final Fluidlog event = new Fluidlog(world, pos, state, fluidState, checkVaporize, flags);
            if(MinecraftForge.EVENT_BUS.post(event)) return Pair.of(event, Result.DENY);
            else return Pair.of(event, event.getResult());
        }
    }

    //CURRENTLY USED, DUE TO PERFORMANCE IMPACT
    //if cancelled, the block can't be fluidlogged
    //result default = default checker; result allow = block is fluidloggable; result deny = block can't be fluidlogged
    @Cancelable
    @HasResult
    public static class Fluidloggable extends FluidloggedEvent
    {
        @Nonnull  public final IBlockState state;
        @Nullable public final Fluid fluid;

        public Fluidloggable(@Nonnull IBlockState state, @Nullable Fluid fluid) {
            this.state = state;
            this.fluid = fluid;
        }
    }
}
