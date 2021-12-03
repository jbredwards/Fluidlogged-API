package git.jbredwards.fluidlogged_api.common.util;

import git.jbredwards.fluidlogged_api.Main;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggableBase;
import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.config.FluidloggedConfig;
import git.jbredwards.fluidlogged_api.common.event.FluidloggedEvent;
import git.jbredwards.fluidlogged_api.common.network.FluidStateMessage;
import git.jbredwards.fluidlogged_api.common.network.NetworkHandler;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
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
        //if instanceof World, check the chunk capability for a stored fluid
        final @Nullable Chunk chunk = getChunk(world, pos);

        if(chunk != null) {
            final IBlockState state = chunk.getBlockState(pos);
            //return fluid if present, else return state
            return getFluidFromState(state) == null ?
                    Optional.ofNullable(FluidState.getFromProvider(chunk, pos).getState()).orElse(state)
                    : state;
        }
        //default
        else return world.getBlockState(pos);
    }

    public static boolean setFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nullable IBlockState state, @Nonnull FluidState fluidState, boolean checkVaporize, int flags) {
        if(world.isOutsideBuildHeight(pos) || world.getWorldType() == WorldType.DEBUG_ALL_BLOCK_STATES) return false;

        final Chunk chunk = world.getChunkFromBlockCoords(pos);
        if(state == null) state = chunk.getBlockState(pos);

        final FluidloggedEvent event = new FluidloggedEvent(world, pos, state, fluidState, checkVaporize, flags);
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
                //preserve old fluid state prior to updating
                final FluidState oldFluidState = cap.getFluidState(pos);

                //check for IFluidloggable
                if(state.getBlock() instanceof IFluidloggable) {
                    final Event.Result result = ((IFluidloggable)state.getBlock()).onFluidChange(world, pos, state, oldFluidState.getFluid(), fluid);
                    if(result != Event.Result.DEFAULT) return result == Event.Result.ALLOW;
                }

                //only do these on server
                if(!world.isRemote) {
                    //send changes to server
                    cap.setFluidState(pos, fluidState);

                    //send changes to client
                    //if((event.flags & Constants.BlockFlags.SEND_TO_CLIENTS) != 0) {
                        NetworkHandler.WRAPPER.sendToAllAround(new FluidStateMessage(pos, fluidState),
                                new NetworkRegistry.TargetPoint(world.provider.getDimension(), pos.getX(), pos.getY(), pos.getZ(), 64)
                        );
                    //}

                    //post fluid added
                    if(!fluidState.isEmpty())
                        fluidState.getState().getBlock().onBlockAdded(world, pos, fluidState.getState());
                }

                //update world properly
                world.markAndNotifyBlock(pos, chunk,
                        Optional.ofNullable(oldFluidState.getState()).orElse(Blocks.AIR.getDefaultState()),
                        Optional.ofNullable(fluidState.getState()).orElse(Blocks.AIR.getDefaultState()), event.flags);
            }

            //default
            return true;
        }
    }

    //fork of Main.CommonProxy#getChunk
    @Nullable
    public static Chunk getChunk(@Nullable IBlockAccess world, @Nonnull BlockPos pos) { return Main.proxy.getChunk(world, pos); }

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

    @Nullable
    public static Fluid getFluidAt(@Nullable IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
        return Optional.ofNullable(getFluidFromState(here)).orElse(FluidState.get(world, pos).getFluid());
    }

    @SuppressWarnings("deprecation")
    public static boolean isFluidFluidloggable(@Nonnull Block fluid) {
        //if the block is already occupied, or has a tile entity, it's not fluidloggable
        if(fluid instanceof IFluidloggableBase || fluid.hasTileEntity()) return false;
        //allow any vanilla fluid block while restricting forge fluids only to BlockFluidClassic
        else return (fluid instanceof BlockLiquid || fluid instanceof BlockFluidClassic);
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
