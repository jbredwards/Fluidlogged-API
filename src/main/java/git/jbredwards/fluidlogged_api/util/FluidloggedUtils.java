package git.jbredwards.fluidlogged_api.util;

import git.jbredwards.fluidlogged_api.common.block.AbstractFluidloggedBlock;
import git.jbredwards.fluidlogged_api.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.block.TileEntityFluidlogged;
import git.jbredwards.fluidlogged_api.common.event.FluidloggedEvent;
import mcp.MethodsReturnNonnullByDefault;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.fml.common.eventhandler.Event;
import net.minecraftforge.items.ItemHandlerHelper;

import javax.annotation.Nullable;
import javax.annotation.ParametersAreNonnullByDefault;

/**
 * helpful functions
 * @author jbred
 *
 */
@MethodsReturnNonnullByDefault
@ParametersAreNonnullByDefault
public enum FluidloggedUtils
{
    ;

    //returns the stored fluidlogged block, null if there is none
    @Nullable
    public static IBlockState getStored(IBlockAccess world, BlockPos pos) {
        final @Nullable TileEntity te = world.getTileEntity(pos);

        if(!(te instanceof TileEntityFluidlogged)) return null;
        else return ((TileEntityFluidlogged)te).stored;
    }

    //should be used instead of world.getBlockState wherever possible
    public static IBlockState getStoredOrReal(IBlockAccess world, BlockPos pos) {
        final IBlockState here = world.getBlockState(pos);

        if(!(here.getBlock() instanceof BlockFluidloggedTE)) return here;
        else return ((BlockFluidloggedTE)here.getBlock()).getStored(world, pos);
    }

    //if the block is fluidlogged, replaces the stored block, else does world.setBlockState()
    //this should be used instead of world.setBlockState wherever possible
    public static void setStoredOrReal(World world, BlockPos pos, @Nullable IBlockState here, @Nullable IBlockState state, boolean notify) {
        final @Nullable IBlockState stored = getStored(world, pos);
        if(state == null) state = Blocks.AIR.getDefaultState();

        //block here isn't fluidlogged
        if(stored == null) {
            world.setBlockState(pos, state, notify ? 3 : 0);
            return;
        }

        //if the here state is null
        if(here == null) here = world.getBlockState(pos);
        if(here.getBlock() instanceof BlockFluidloggedTE) {
            //if the state to be set is air and the block here is fluidlogged, set to the fluid
            if(state.getBlock() == Blocks.AIR) {
                world.setBlockState(pos, ((BlockFluidloggedTE)here.getBlock()).fluid.getBlock().getDefaultState(), notify ? 3 : 0);
                return;
            }

            //if the state to be set can't be fluidlogged
            if(!isStateFluidloggable(state, null)) {
                world.setBlockState(pos, state, notify ? 3 : 0);
                return;
            }

            ((BlockFluidloggedTE)here.getBlock()).setStored(world, pos, state, notify);
        }
        else world.setBlockState(pos, state, notify ? 3 : 0);
    }

    //checks if the block can be fluidlogged at all
    public static boolean isStateFluidloggable(@Nullable IBlockState state) {
        return isStateFluidloggable(state, null);
    }

    //fluid is null if checking is the block can be fluidlogged at all
    //returns true if the state can be fluidlogged with the given fluid
    public static boolean isStateFluidloggable(@Nullable IBlockState state, @Nullable Fluid fluid) {
        if(state == null) return false;

        final FluidloggedEvent.CheckFluidloggable event = new FluidloggedEvent.CheckFluidloggable(state, fluid);

        //event did stuff
        if(MinecraftForge.EVENT_BUS.post(event)) return false;
        else if(event.getResult() == Event.Result.DENY) return false;
        else if(event.getResult() == Event.Result.ALLOW) return true;
        //default
        else {
            final Block block = state.getBlock();

            //modded
            if(block instanceof IFluidloggable) return ((IFluidloggable)block).isFluidValid(state, fluid);
            //unsupported tile entity blocks
            else if(block.hasTileEntity(state)) return false;
            //normal vanilla blocks
            else return (block instanceof BlockSlab && !((BlockSlab)block).isDouble())
                        || block instanceof BlockStairs
                        || block instanceof BlockPane
                        || block instanceof BlockFence
                        || block instanceof BlockEndRod
                        || block instanceof BlockWall
                        || block instanceof BlockBarrier
                        || block instanceof BlockLeaves
                        || block instanceof BlockFenceGate
                        || block instanceof BlockTrapDoor
                        || block instanceof BlockLadder
                        || block instanceof BlockRailBase;
        }
    }

    //use state sensitive version
    public static boolean tryFluidlogBlock(World world, BlockPos pos, Fluid fluid, boolean ignoreVaporize) {
        return tryFluidlogBlock(world, pos, world.getBlockState(pos), fluid, ignoreVaporize);
    }

    //same as below method, but also calls isStateFluidloggable
    public static boolean tryFluidlogBlock(World world, BlockPos pos, @Nullable IBlockState here, Fluid fluid, boolean ignoreVaporize) {
        return tryFluidlogBlock(world, pos, here, fluid, ignoreVaporize, isStateFluidloggable(here, fluid));
    }

    //tries to fluidlog the block here with the fluid
    //returns true if the block was successfully fluidlogged
    public static boolean tryFluidlogBlock(World world, BlockPos pos, @Nullable IBlockState here, Fluid fluid, boolean ignoreVaporize, boolean isStateFluidloggable) {
        if(!isStateFluidloggable) return false;
        if(here == null) here = world.getBlockState(pos);

        final BlockFluidloggedTE block = FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.get(fluid);
        //shouldn't be null, but just in case
        if(block != null) {
            block.updateQuanta();
            final IBlockState stored = (here.getBlock() instanceof IFluidloggable ? ((IFluidloggable)here.getBlock()).getFluidloggedState(world, pos, here) : here);
            final FluidloggedEvent.Fluidlog event = new FluidloggedEvent.Fluidlog(world, pos, here, stored, block, new TileEntityFluidlogged(), ignoreVaporize);

            //event did stuff
            if(MinecraftForge.EVENT_BUS.post(event)) return false;
            else if(event.getResult() == Event.Result.DENY) return false;
            else if(event.getResult() == Event.Result.ALLOW) return true;
            //default
            else {
                //vaporizes water if in the nether
                if(!event.ignoreVaporize && world.provider.doesWaterVaporize() && fluid.doesVaporize(new FluidStack(fluid, Fluid.BUCKET_VOLUME))) {
                    fluid.vaporize(null, world, pos, new FluidStack(fluid, Fluid.BUCKET_VOLUME));
                    return false;
                }

                if(!world.isRemote) {
                    event.te.setStored(event.stored, false);
                    world.setBlockState(pos, event.block.getDefaultState());
                    world.setTileEntity(pos, event.te);
                    event.stored.getBlock().onBlockAdded(world, pos, event.stored);
                }

                return true;
            }
        }

        return false;
    }

    //use state sensitive version
    public static boolean tryUnfluidlogBlock(World world, BlockPos pos) {
        return tryUnfluidlogBlock(world, pos, null, null);
    }

    //use stored sensitive version
    public static boolean tryUnfluidlogBlock(World world, BlockPos pos, @Nullable IBlockState here) {
        return tryUnfluidlogBlock(world, pos, here, null);
    }

    //tries to un-fluidlog the block here
    //returns true if the block was successfully un-fluidlogged
    public static boolean tryUnfluidlogBlock(World world, BlockPos pos, @Nullable IBlockState here, @Nullable IBlockState stored) {
        final @Nullable TileEntity te = world.getTileEntity(pos);
        if(te instanceof TileEntityFluidlogged) {
            if(stored == null) stored = ((TileEntityFluidlogged)te).stored;
            if(here == null) here = world.getBlockState(pos);

            final IBlockState toCreate = (stored.getBlock() instanceof IFluidloggable ? ((IFluidloggable)stored.getBlock()).getNonFluidloggedState(world, pos, stored) : stored);
            final FluidloggedEvent.UnFluidlog event = new FluidloggedEvent.UnFluidlog(world, pos, here, stored, (TileEntityFluidlogged)te, toCreate);

            //event did stuff
            if(MinecraftForge.EVENT_BUS.post(event)) return false;
            else if(event.getResult() == Event.Result.DENY) return false;
            else if(event.getResult() == Event.Result.ALLOW) return true;
            //default
            else {
                if(!world.isRemote) world.setBlockState(pos, event.toCreate);
                return true;
            }
        }

        //block here isn't fluidlogged
        return false;
    }

    //fills the bucket stack with the fluid
    public static ItemStack getFilledBucket(ItemStack empty, Fluid fluid) {
        empty = ItemHandlerHelper.copyStackWithSize(empty, 1);
        final @Nullable IFluidHandlerItem handler = FluidUtil.getFluidHandler(empty);

        if(handler != null) {
            handler.fill(new FluidStack(fluid, Fluid.BUCKET_VOLUME), true);
            return handler.getContainer();
        }

        //not a bucket
        return empty;
    }

    //empties the bucket stack
    public static ItemStack getEmptyBucket(ItemStack filled) {
        filled = ItemHandlerHelper.copyStackWithSize(filled, 1);
        final @Nullable IFluidHandlerItem handler = FluidUtil.getFluidHandler(filled);

        if(handler != null) {
            handler.drain(Fluid.BUCKET_VOLUME, true);
            return handler.getContainer();
        }

        //not a bucket
        return filled;
    }

    //gets the fluid from the block (null if there is no fluid)
    @Nullable
    public static Fluid getFluidFromBlock(Block fluid) {
        //modded
        if(fluid instanceof IFluidBlock) return ((IFluidBlock)fluid).getFluid();
        //vanilla
        else if(fluid.getDefaultState().getMaterial() == Material.WATER) return FluidRegistry.WATER;
        else return fluid.getDefaultState().getMaterial() == Material.LAVA ? FluidRegistry.LAVA : null;
    }

    //returns true if the fluid can create sources
    public static boolean canFluidCreateSources(Fluid fluid) {
        final @Nullable Block block = fluid.getBlock();
        if(block == null) return false;

        //modded
        if(block instanceof BlockFluidClassic) {
            try { return AbstractFluidloggedBlock.canCreateSourcesField.getBoolean(block); }
            catch(Exception ignored) { /* default return */ }
        }
        else if(block instanceof BlockFluidFinite) return false;
        //default
        return block.getDefaultState().getMaterial() == Material.WATER;
    }

    //returns true if the block here is a fluid source block
    public static boolean isSource(IBlockAccess world, BlockPos pos, @Nullable IBlockState here) {
        if(here == null) here = world.getBlockState(pos);

        //modded
        if(here.getBlock() instanceof BlockFluidClassic) return ((BlockFluidClassic)here.getBlock()).isSourceBlock(world, pos);
        //vanilla
        else if(here.getBlock() instanceof BlockLiquid) return here.getValue(BlockLiquid.LEVEL) == 0;

        //default
        return false;
    }
}
