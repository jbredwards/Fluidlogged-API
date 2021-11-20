package git.jbredwards.fluidlogged_api.common.util;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.config.FluidloggedConfig;
import git.jbredwards.fluidlogged_api.common.event.FluidloggedEvent;
import git.jbredwards.fluidlogged_api.common.network.FluidStateMessage;
import git.jbredwards.fluidlogged_api.common.network.NetworkHandler;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.ChunkCache;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.IFluidBlock;
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
        //if the chuck isn't accessible, get the fluid from the world
        final @Nullable Chunk chunk = getChunk(world, pos);
        if(chunk == null) return convertToFluidState(world.getBlockState(pos), false);
        //if the chunk is accessible, get the capability if state isn't fluid
        final @Nullable IBlockState state = convertToFluidState(chunk.getBlockState(pos), false);
        return state == null ? FluidState.getFromProvider(chunk, pos).getState() : state;
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
            return getFluidFromBlock(state.getBlock()) == null ?
                    Optional.ofNullable(FluidState.getFromProvider(chunk, pos).getState()).orElse(state)
                    : state;
        }
        //default
        else return world.getBlockState(pos);
    }

    //converts the input state into the parent's fluid state (with the level prop carried over)
    //returns null if the state is not a fluid
    @Nullable
    public static IBlockState convertToFluidState(@Nonnull IBlockState state, boolean ignoreFluidLevel) {
        final @Nullable Fluid fluid = getFluidFromBlock(state.getBlock());
        if(fluid != null) {
            final @Nullable Block block = state.getBlock() instanceof BlockLiquid ?
                    BlockLiquid.getFlowingBlock(state.getMaterial()) : fluid.getBlock();

            if(block != null) return ignoreFluidLevel ? block.getDefaultState() :
                    block.getDefaultState().withProperty(BlockLiquid.LEVEL, state.getValue(BlockLiquid.LEVEL));
        }
        //default
        return null;
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
                    if((event.flags & Constants.BlockFlags.SEND_TO_CLIENTS) != 0) {
                        NetworkHandler.WRAPPER.sendToAllAround(new FluidStateMessage(pos, fluidState),
                                new NetworkRegistry.TargetPoint(world.provider.getDimension(), pos.getX(), pos.getY(), pos.getZ(), 64)
                        );
                    }

                    //post fluid added
                    if(!fluidState.isEmpty())
                        fluidState.getState().getBlock().onBlockAdded(world, pos, fluidState.getState());
                }

                //update world properly
                world.markAndNotifyBlock(pos, chunk,
                        Optional.ofNullable(oldFluidState.getState()).orElse(Blocks.AIR.getDefaultState()),
                        Optional.ofNullable(fluidState.getState()).orElse(Blocks.AIR.getDefaultState()), event.flags
                );
            }

            //default
            return true;
        }
    }

    //tries to get the chunk from IBlockAccess
    @Nullable
    public static Chunk getChunk(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        //client instance is always used by the client
        try { return Minecraft.getMinecraft().world.getChunkFromBlockCoords(pos); }
        catch(Throwable throwable) {
            if(world instanceof World) return ((World)world).getChunkFromBlockCoords(pos);
            else if(world instanceof ChunkCache) return ((ChunkCache)world).world.getChunkFromBlockCoords(pos);
            else if(world instanceof IChunkProvider) return ((IChunkProvider)world).getChunkFromBlockCoords(pos);
            else return null;
        }
    }

    //gets the fluid from the block (null if there is no fluid)
    @Nullable
    public static Fluid getFluidFromBlock(@Nonnull Block fluid) {
        //modded
        if(fluid instanceof IFluidBlock) return ((IFluidBlock)fluid).getFluid();
        //vanilla
        else if(fluid.getDefaultState().getMaterial() == Material.WATER) return FluidRegistry.WATER;
        else return fluid.getDefaultState().getMaterial() == Material.LAVA ? FluidRegistry.LAVA : null;
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
                    || block instanceof BlockEnderChest;
        }
    }
}
