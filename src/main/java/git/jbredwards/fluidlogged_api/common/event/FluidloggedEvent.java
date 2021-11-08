package git.jbredwards.fluidlogged_api.common.event;

import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public abstract class FluidloggedEvent extends Event
{
    public static abstract class Fluidlog extends FluidloggedEvent
    {
        @Nonnull  public final World world;
        @Nonnull  public final BlockPos pos;
        @Nonnull  public final IBlockState state;
        @Nullable public IBlockState fluidState;
        public final boolean notify;
        public final boolean sendToClient;
        public boolean checkVaporize;

        public Fluidlog(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable IBlockState fluidState, boolean notify, boolean sendToClient, boolean checkVaporize) {
            this.world = world;
            this.pos = pos;
            this.state = state;
            this.fluidState = fluidState;
            this.notify = notify;
            this.sendToClient = sendToClient;
            this.checkVaporize = checkVaporize;
        }

        @Nullable
        public Fluid getFluid() { return fluidState == null ? null : FluidloggedUtils.getFluidFromBlock(fluidState.getBlock()); }

        //fired when trying to fluidlog a block
        //if cancelled, the block will not become fluidlogged
        //result default = default code gets ran; result allow = block was fluidlogged; result deny = block was not fluidlogged
        @Cancelable
        @HasResult
        public static class Pre extends Fluidlog
        {
            public Pre(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable IBlockState fluidState, boolean notify, boolean sendToClient, boolean checkVaporize) {
                super(world, pos, state, fluidState, notify, sendToClient, checkVaporize);
            }
        }

        //fired after a block is fluidlogged
        //if cancelled, the bucket will not be drained
        @Cancelable
        public static class Post extends Fluidlog
        {
            public Post(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable IBlockState fluidState, boolean notify, boolean sendToClient, boolean checkVaporize) {
                super(world, pos, state, fluidState, notify, sendToClient, checkVaporize);
            }
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
