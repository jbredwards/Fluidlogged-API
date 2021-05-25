package git.jbredwards.fluidlogged.common.event;

import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * you should still use IFluidloggable for your own blocks if possible
 * @author jbred
 *
 */
public abstract class FluidloggedEvent extends Event
{
    //fired when trying to fluidlog a block
    //if cancelled, the block will not become fluidlogged
    //result default = default code gets ran; result allow = block was fluidlogged, default code does not get ran; result deny = block was not fluidlogged, default code does not get ran
    @HasResult
    @Cancelable
    public static class Fluidlog extends FluidloggedEvent
    {
        @Nonnull public final World world;
        @Nonnull public final BlockPos pos;
        //block prior to fluidlog
        @Nonnull public final IBlockState here;
        //block to be fluidlogged
        @Nonnull public IBlockState stored;
        //block that will store the fluidlogged data
        @Nonnull public BlockFluidloggedTE block;
        //tile entity
        @Nonnull public TileEntityFluidlogged te;
        //false if the event is dimension sensitive
        //(for example, this is true when the player tries to place a fluidloggable block into an already existing fluid)
        public boolean ignoreVaporize;

        public Fluidlog(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull IBlockState stored, @Nonnull BlockFluidloggedTE block, @Nonnull TileEntityFluidlogged te, boolean ignoreVaporize) {
            this.world = world;
            this.pos = pos;
            this.here = here;
            this.stored = stored;
            this.block = block;
            this.te = te;
            this.ignoreVaporize = ignoreVaporize;
        }
    }

    //fired when trying to un-fluidlog a block
    //if cancelled, the block will remain fluidlogged
    //result default = default code gets ran; result allow = block was un-fluidlogged, default code does not get ran; result deny = block was not un-fluidlogged, default code does not get ran
    @HasResult
    @Cancelable
    public static class UnFluidlog extends FluidloggedEvent
    {
        @Nonnull public final World world;
        @Nonnull public final BlockPos pos;
        @Nonnull public final IBlockState here;
        //stored block prior to un-fluidlog
        @Nonnull public final IBlockState stored;
        //block that gets made when un-fluidlog
        @Nonnull public IBlockState toCreate;

        public UnFluidlog(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull IBlockState stored, @Nonnull IBlockState toCreate) {
            this.world = world;
            this.pos = pos;
            this.here = here;
            this.stored = stored;
            this.toCreate = toCreate;
        }
    }

    //fired by FluidloggedUtils.isStateFluidloggable(state, fluid)
    //if cancelled, the block can't be fluidlogged
    //result default = default checker; result allow = block is fluidloggable; result deny = block can't be fluidlogged
    @HasResult
    @Cancelable
    public static class CheckFluidloggable extends FluidloggedEvent
    {
        @Nonnull public final IBlockState state;
        @Nullable public final Fluid fluid;

        public CheckFluidloggable(@Nonnull IBlockState state, @Nullable Fluid fluid) {
            this.state = state;
            this.fluid = fluid;
        }
    }
}
