package git.jbredwards.fluidlogged.common.event;

import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;

/**
 * you should still use IFluidloggable for your own blocks
 * these exist in-case you want to change something about vanilla
 * @author jbred
 *
 */
public abstract class FluidloggedEvent extends Event
{
    public final World world;
    public final BlockPos pos;

    public FluidloggedEvent(World world, BlockPos pos) {
        this.world = world;
        this.pos = pos;
    }

    //fired when trying to fluidlog a block
    //if cancelled, the block will not become fluidlogged
    @Cancelable
    public static class Fluidlog extends FluidloggedEvent
    {
        //block prior to fluidlog
        @Nonnull public final IBlockState here;
        //block to be fluidlogged
        @Nonnull public IBlockState stored;
        //block that stores the fluidlogged data
        @Nonnull public BlockFluidloggedTE block;
        //tile entity
        @Nonnull public TileEntityFluidlogged te;


        public Fluidlog(World world, BlockPos pos, IBlockState here, IBlockState stored, BlockFluidloggedTE block, TileEntityFluidlogged te) {
            super(world, pos);
            this.here = here;
            this.stored = stored;
            this.block = block;
            this.te = te;
        }
    }

    //fired when trying to un-fluidlog a block
    //if cancelled, the block will remain fluidlogged
    @Cancelable
    public static class UnFluidlog extends FluidloggedEvent
    {
        //stored block prior to un-fluidlog
        @Nonnull public final IBlockState stored;
        //block that gets made when un-fluidlog
        @Nonnull public IBlockState toCreate;
        //block that stores the fluidlogged data
        @Nonnull public BlockFluidloggedTE block;

        public UnFluidlog(World world, BlockPos pos, IBlockState stored, IBlockState toCreate, BlockFluidloggedTE block) {
            super(world, pos);
            this.stored = stored;
            this.toCreate = toCreate;
            this.block = block;
        }
    }
}
