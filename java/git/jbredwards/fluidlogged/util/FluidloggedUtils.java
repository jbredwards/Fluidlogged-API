package git.jbredwards.fluidlogged.util;

import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.common.block.IFluidloggable;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import net.minecraft.block.*;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.Fluid;

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

    public static void setStored(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nullable IBlockState stored, boolean notify) {

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
}
