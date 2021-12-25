package git.jbredwards.fluidlogged_api.common.util;

import git.jbredwards.fluidlogged_api.Main;
import git.jbredwards.fluidlogged_api.asm.replacements.BlockLiquidBase;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.config.FluidloggedConfig;
import git.jbredwards.fluidlogged_api.common.event.FluidloggedEvent;
import git.jbredwards.fluidlogged_api.common.network.FluidStateMessage;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fml.common.eventhandler.Event;
import net.minecraftforge.fml.common.network.NetworkRegistry;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Optional;

/**
 * helpful functions
 * @author jbred
 *
 */
public enum FluidloggedUtils
{
    ;

    //if you want the FluidState directly, use FluidState#of
    @Nullable
    public static IBlockState getFluidState(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final @Nullable Chunk chunk = getChunk(world, pos);
        final IBlockState here = chunk == null ? world.getBlockState(pos) : chunk.getBlockState(pos);
        //if block here is a fluid, return it
        if(getFluidFromState(here) != null) return here;
        //else return stored FluidState if present
        else return chunk == null ? null : FluidState.getFromProvider(chunk, pos).getState();
    }

    //tries to get the fluid at the pos (prioritizing ones physically in the world, then the fluid capability),
    //if none return world#getBlockState
    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return getFluidOrReal(world, pos, world.getBlockState(pos));
    }

    //same as above method, but takes in the here state rather than getting it from the world
    //(useful for avoiding unnecessary lookups)
    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
        return getFluidFromState(here) != null ? here : Optional.ofNullable(FluidState.get(world, pos).getState()).orElse(here);
    }

    public static boolean setFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nullable IBlockState here, @Nonnull FluidState fluidState, boolean checkVaporize, int flags) {
        if(world.isOutsideBuildHeight(pos) || world.getWorldType() == WorldType.DEBUG_ALL_BLOCK_STATES) return false;

        final Chunk chunk = world.getChunkFromBlockCoords(pos);
        if(here == null) here = chunk.getBlockState(pos);

        final FluidloggedEvent event = new FluidloggedEvent(world, chunk, pos, here, fluidState, checkVaporize, flags);
        //event did stuff
        if(MinecraftForge.EVENT_BUS.post(event) || event.getResult() != Event.Result.DEFAULT) return event.getResult() == Event.Result.ALLOW;
        //default
        else {
            //sync possible event changes
            fluidState = event.fluidState;
            final @Nullable Fluid fluid = fluidState.getFluid();

            //if the world is to warm for the fluid, vaporize it
            if(event.checkVaporize && fluid != null && world.provider.doesWaterVaporize() && fluid.doesVaporize(new FluidStack(fluid, Fluid.BUCKET_VOLUME))) {
                fluid.vaporize(null, world, pos, new FluidStack(fluid, Fluid.BUCKET_VOLUME));
                return true;
            }

            @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
            if(cap != null) {
                //check for IFluidloggable
                if(here.getBlock() instanceof IFluidloggable) {
                    final Event.Result result = ((IFluidloggable)here.getBlock()).onFluidChange(world, pos, here, cap.getFluidState(pos).getFluid(), fluid, event.flags);
                    if(result != Event.Result.DEFAULT) return result == Event.Result.ALLOW;
                }

                //moved to separate function, as to allow easy calling by event instances, and IFluidloggable instances using IFluidloggable#onFluidChange
                setFluidState_Internal(world, chunk, here, cap, pos, fluidState, event.flags);
            }

            //default
            return true;
        }
    }

    //if you're not an event instance or an IFluidloggable instance, use setFluidState instead!
    //moved to separate function, as to allow easy calling by event instances, and IFluidloggable instances using IFluidloggable#onFluidChange
    public static void setFluidState_Internal(@Nonnull World world, @Nonnull Chunk chunk, @Nonnull IBlockState here, @Nonnull IFluidStateCapability cap, @Nonnull BlockPos pos, @Nonnull FluidState fluidState, int flags) {
        //fix small graphical flicker with blocks placed inside fluids
        if(world.isRemote && !fluidState.isEmpty()) cap.setFluidState(pos, fluidState);

        //only do these on server
        if(!world.isRemote) {
            //send changes to server
            cap.setFluidState(pos, fluidState);

            //send changes to client
            Main.wrapper.sendToAllAround(new FluidStateMessage(pos, fluidState),
                    new NetworkRegistry.TargetPoint(world.provider.getDimension(), pos.getX(), pos.getY(), pos.getZ(), 64)
            );

            //update light levels
            world.profiler.startSection("checkLight");
            world.checkLight(pos);
            world.profiler.endSection();

            //post fluid added
            if(!fluidState.isEmpty()) fluidState.getBlock().onBlockAdded(world, pos, fluidState.getState());
        }

        //update world
        world.markAndNotifyBlock(pos, chunk, here, here, flags);
    }

    //fork of Main.CommonProxy#getChunk
    @Nullable
    public static Chunk getChunk(@Nullable IBlockAccess world, @Nonnull BlockPos pos) { return Main.proxy.getChunk(world, pos); }

    //fork of IFluidloggable#canFluidFlow
    public static boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nullable Fluid fluid, @Nonnull EnumFacing side) {
        if(state.getBlock() instanceof IFluidloggable) return ((IFluidloggable)state.getBlock()).canFluidFlow(world, pos, state, fluid, side);
        else return state.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    //gets the fluid from the state (null if there is no fluid)
    @Nullable
    public static Fluid getFluidFromState(@Nonnull IBlockState fluid) { return getFluidFromBlock(fluid.getBlock()); }

    //gets the fluid from the block (null if there is no fluid)
    @Nullable
    public static Fluid getFluidFromBlock(@Nonnull Block fluid) {
        //modded
        if(fluid instanceof IFluidBlock) return ((IFluidBlock)fluid).getFluid();
        //vanilla
        else if(fluid.getDefaultState().getMaterial() == Material.WATER) return FluidRegistry.WATER;
        else return fluid.getDefaultState().getMaterial() == Material.LAVA ? FluidRegistry.LAVA : null;
    }

    //helpful utility function to get the fluid from the state, but fallback on the possible FluidState here
    @Nullable
    public static Fluid getFluidAt(@Nullable IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
        return Optional.ofNullable(getFluidFromState(here)).orElse(FluidState.get(world, pos).getFluid());
    }

    @SuppressWarnings("deprecation")
    public static boolean isFluidFluidloggable(@Nonnull Block fluid) {
        //allow any vanilla fluid block while restricting forge fluids only to BlockFluidClassic
        return !fluid.hasTileEntity() && (fluid instanceof BlockLiquidBase || fluid instanceof BlockFluidClassic);
    }

    //same as above method, but also checks for fluid level
    public static boolean isFluidFluidloggable(@Nonnull IBlockState fluid) {
        if(!isFluidFluidloggable(fluid.getBlock())) return false;
        final int level = fluid.getValue(BlockLiquidBase.LEVEL);
        return level == 0 || (level >= 8 && AccessorUtils.canCreateSources(fluid.getBlock()));
    }

    public static boolean isStateFluidloggable(@Nonnull IBlockState state, @Nullable Fluid fluid) {
        //config
        final Event.Result configResult = FluidloggedConfig.isStateFluidloggable(state, fluid);
        if(configResult != Event.Result.DEFAULT) return configResult == Event.Result.ALLOW;
        //defaults
        else {
            //modded
            final Block block = state.getBlock();
            if(block instanceof IFluidloggable) {
                if(fluid == null) return ((IFluidloggable)block).isFluidloggable(state);
                else              return ((IFluidloggable)block).isFluidValid(state, fluid);
            }
            //vanilla
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
                    || block instanceof BlockRailBase
                    || block instanceof BlockHopper
                    || block instanceof BlockChest
                    || block instanceof BlockEnderChest
                    || block instanceof BlockSkull;
        }
    }
}
