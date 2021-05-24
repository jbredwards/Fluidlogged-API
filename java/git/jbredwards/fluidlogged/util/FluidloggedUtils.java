package git.jbredwards.fluidlogged.util;

import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.common.block.IFluidloggable;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import net.minecraft.block.*;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.items.ItemHandlerHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * helpful functions
 * @author jbred
 *
 */
public enum FluidloggedUtils
{
    ;

    //returns the stored fluidlogged block, null if there is none
    @Nullable
    public static IBlockState getStored(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final @Nullable TileEntity te = world.getTileEntity(pos);

        if(!(te instanceof TileEntityFluidlogged)) return null;
        else return ((TileEntityFluidlogged)te).getStored();
    }

    //if the block is fluidlogged, replaces the stored block, else does world.setBlockState()
    public static void setStoredOrReal(@Nonnull World world, @Nonnull BlockPos pos, @Nullable IBlockState here, @Nullable IBlockState state, boolean notify) {
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
            if(!isStateFluidloggable(state)) {
                world.setBlockState(pos, state, notify ? 3 : 0);
                return;
            }

            ((BlockFluidloggedTE)here.getBlock()).setStored(world, pos, state, notify);
        }
        else world.setBlockState(pos, state, notify ? 3 : 0);
    }

    //should be used instead of world.getBlockState wherever possible
    public static IBlockState getStoredOrReal(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState here = world.getBlockState(pos);

        if(!(here.getBlock() instanceof BlockFluidloggedTE)) return here;
        else return getStored(world, pos);
    }

    //returns true if the state can be fluidlogged
    public static boolean isStateFluidloggable(@Nullable IBlockState state) {
        if(state == null) return false;
        final Block block = state.getBlock();

        //modded
        if(block instanceof IFluidloggable) return true;
            //vanilla tile entity blocks
        else if(block instanceof ITileEntityProvider) return false;
            //normal vanilla blocks
        else return (block instanceof BlockSlab && !((BlockSlab)block).isDouble())
                    || block instanceof BlockStairs
                    || block instanceof BlockPane
                    || block instanceof BlockFence
                    || block instanceof BlockEndRod
                    || block instanceof BlockWall
                    || block instanceof BlockBarrier
                    || block instanceof BlockLeaves;
    }

    //returns true if the state can be fluidlogged with the given fluid
    public static boolean isStateFluidloggable(@Nullable IBlockState state, @Nullable Fluid fluid) {
        if(state == null) return false;
        if(state.getBlock() instanceof IFluidloggable) return ((IFluidloggable)state.getBlock()).isFluidValid(state, fluid);
        else return isStateFluidloggable(state);
    }

    //fills the bucket stack with the fluid
    public static ItemStack getFilledBucket(@Nonnull ItemStack empty, @Nonnull Fluid fluid) {
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
    public static ItemStack getEmptyBucket(@Nonnull ItemStack filled) {
        filled = ItemHandlerHelper.copyStackWithSize(filled, 1);
        final @Nullable IFluidHandlerItem handler = FluidUtil.getFluidHandler(filled);

        if(handler != null) {
            handler.drain(Fluid.BUCKET_VOLUME, true);
            return handler.getContainer();
        }

        //not a bucket
        return filled;
    }
}
