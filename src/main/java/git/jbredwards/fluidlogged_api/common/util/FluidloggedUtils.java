package git.jbredwards.fluidlogged_api.common.util;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggableBase;
import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.event.FluidloggedEvent;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.IFluidBlock;
import net.minecraftforge.fml.common.eventhandler.Event;

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

    @Nullable
    public static IBlockState getFluidState(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        //if instanceof World, check the chunk capability for a stored fluid
        if(world instanceof World) {
            final Chunk chunk = ((World)world).getChunkFromBlockCoords(pos);
            final @Nullable IBlockState state = convertToFluidState(chunk.getBlockState(pos));
            //get from capability
            if(state == null) {
                final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
                return cap == null ? null : cap.getFluidState(pos);
            }
            //get from fluid in the world
            return state;
        }
        //default
        else return convertToFluidState(world.getBlockState(pos));
    }

    //tries to get the fluid at the pos (prioritizing ones physically in the world, then the fluid capability),
    //if none return world#getBlockState
    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        //if instanceof World, check the chunk capability for a stored fluid
        if(world instanceof World) {
            final Chunk chunk = ((World)world).getChunkFromBlockCoords(pos);
            final IBlockState state = chunk.getBlockState(pos);
            //get from capability
            if(getFluidFromBlock(state.getBlock()) == null) {
                final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
                final @Nullable IBlockState fluid = cap == null ? null : cap.getFluidState(pos);
                return fluid == null ? state : fluid;
            }

            return state;
        }
        //default
        else return world.getBlockState(pos);
    }

    //converts the input state into the parent's fluid state (with the level prop carried over)
    //returns null if the state is not a fluid
    @Nullable
    public static IBlockState convertToFluidState(@Nonnull IBlockState state) {
        final @Nullable Fluid fluid = getFluidFromBlock(state.getBlock());
        if(fluid != null) {
            final Block block = fluid.getBlock();
            if(block != null) {
                //if the block here matches the fluid block
                if(block.equals(state.getBlock())
                        || state.getBlock() instanceof BlockLiquid
                        && block.equals(BlockLiquid.getFlowingBlock(state.getMaterial()))) return state;
                //fix the level property
                return block.getDefaultState().withProperty(BlockLiquid.LEVEL, state.getValue(BlockLiquid.LEVEL));
            }
        }
        //default
        return null;
    }

    public static boolean setFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nullable IBlockState state, @Nullable IBlockState fluidState, boolean notify, boolean sendToClient, boolean ignoreVaporize) {
        if(world.getWorldType() == WorldType.DEBUG_ALL_BLOCK_STATES) return false;

        final Chunk chunk = world.getChunkFromBlockCoords(pos);
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
        final @Nullable IBlockState old = cap == null ? null : cap.getFluidState(pos);
        //do nothing if the fluid state won't change
        if(fluidState == old) return false;

        if(state == null) state = chunk.getBlockState(pos);
        final FluidloggedEvent.Fluidlog.Pre event = new FluidloggedEvent.Fluidlog.Pre(world, pos, state, fluidState, notify, sendToClient, ignoreVaporize);
        //event did stuff
        if(MinecraftForge.EVENT_BUS.post(event)) return false;
        else if(event.getResult() == Event.Result.DENY) return false;
        else if(event.getResult() == Event.Result.ALLOW) return true;
        //default
        else {
            //sync possible event changes
            final @Nullable Fluid fluid = event.getFluid();
            fluidState = event.fluidState;

            //if the world is to warm for the fluid, vaporize it
            if(fluid != null && !event.ignoreVaporize && world.provider.doesWaterVaporize() && fluid.doesVaporize(new FluidStack(fluid, Fluid.BUCKET_VOLUME))) {
                fluid.vaporize(null, world, pos, new FluidStack(fluid, Fluid.BUCKET_VOLUME));
                return false;
            }



            //default
            return false;
        }
    }

    //gets the fluid from the world or the capability
    @Nullable
    public static Fluid getFluid(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(world instanceof World) {
            final Chunk chunk = ((World)world).getChunkFromBlockCoords(pos);
            final IBlockState state = chunk.getBlockState(pos);
            final @Nullable Fluid fluid = getFluidFromBlock(state.getBlock());
            if(fluid == null) {
                final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
                final @Nullable IBlockState fluidState = cap == null ? null : cap.getFluidState(pos);
                return fluidState == null ? null : getFluidFromBlock(fluidState.getBlock());
            }

            return fluid;
        }
        //default
        return getFluidFromBlock(world.getBlockState(pos).getBlock());
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
        //TODO add config whitelist & blocklist here
        //REMOVED EVENT DUE TO PERFORMANCE HIT
        /*final FluidloggedEvent.Fluidloggable event = new FluidloggedEvent.Fluidloggable(state, fluid);
        //event did stuff
        if(MinecraftForge.EVENT_BUS.post(event)) return false;
        else if(event.getResult() == Event.Result.DENY) return false;
        else if(event.getResult() == Event.Result.ALLOW) return true;
        //default
        else {*/
            final Block block = state.getBlock();
            //modded
            if(block instanceof IFluidloggable) {
                if(fluid == null) return ((IFluidloggable)block).isFluidloggable(state);
                else              return ((IFluidloggable)block).isFluidValid(state, fluid);
            }
            //vanilla supported blocks
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
        //}
    }
}
