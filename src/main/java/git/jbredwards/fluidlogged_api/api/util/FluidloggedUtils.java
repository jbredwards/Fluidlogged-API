/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.util;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.event.FluidloggedEvent;
import git.jbredwards.fluidlogged_api.api.fluid.ICompatibleFluid;
import git.jbredwards.fluidlogged_api.api.network.MessageUtils;
import git.jbredwards.fluidlogged_api.api.world.IChunkProvider;
import git.jbredwards.fluidlogged_api.api.world.IWorldProvider;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.asm.iface.ICanFluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigFluidBox;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginBlockFluidClassic;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import git.jbredwards.fluidlogged_api.mod.common.message.SMessageSyncFluidState;
import git.jbredwards.fluidlogged_api.mod.common.message.SMessageVaporizeEffects;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.chunk.BlockStateContainer;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.IFluidBlock;
import net.minecraftforge.fml.common.eventhandler.Event;
import net.minecraftforge.fml.common.network.NetworkRegistry;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;

/**
 * A utility class containing various functions for getting and setting FluidStates.
 *
 * @since 1.7.0
 * @author jbred
 *
 */
public final class FluidloggedUtils
{
    /**
     * If the state at the position is a fluid, return it as a FluidState. Otherwise, return the FluidState at the position.
     * If you know that the state at the position is not a fluid, use {@link FluidState#get(IBlockAccess, BlockPos) FluidState::get} instead.
     *
     * @param world IBlockAccess.
     * @param pos Position.
     * @return The state at the position as a fluidState (if the state is a fluid). Otherwise, returns the fluidState at the position.
     *
     * @throws NullPointerException if world or pos are null.
     * @since 1.7.0
     */
    @Nonnull
    public static FluidState getFluidState(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
        @Nullable final Chunk chunk = world instanceof IChunkProvider ? ((IChunkProvider)world).getChunk(pos) : null;
        return chunk != null ? getFluidState(chunk, pos) : getFluidState(world, pos, world.getBlockState(pos));
    }

    /**
     * If the state at the position is a fluid, return it as a FluidState. Otherwise, return the FluidState at the position.
     * If you know that the state at the position is not a fluid, use {@link FluidState#get(IBlockAccess, BlockPos) FluidState::get} instead.
     *
     * @param world IBlockAccess.
     * @param pos Position.
     * @param state IBlockState at the position.
     * @return The state at the position as a fluidState (if the state is a fluid). Otherwise, returns the fluidState at the position.
     *
     * @throws NullPointerException if pos is null.
     * @since 1.7.0
     */
    @Nonnull
    public static FluidState getFluidState(@Nullable final IBlockAccess world, @Nonnull final BlockPos pos, @Nullable final IBlockState state) {
        return isFluid(state) ? FluidState.of(state) : FluidState.get(world, pos);
    }

    /**
     * If the state at the position is a fluid, return it as a FluidState. Otherwise, return the FluidState at the position.
     * If you know that the state at the position is not a fluid, use {@link FluidState#getFromProvider(ICapabilityProvider, BlockPos) FluidState::getFromProvider} instead.
     *
     * @param chunk Chunk.
     * @param pos Position.
     * @return The state at the position as a fluidState (if the state is a fluid). Otherwise, returns the fluidState at the position.
     *
     * @throws NullPointerException if chunk or pos are null.
     * @since 3.0.0
     */
    @Nonnull
    public static FluidState getFluidState(@Nonnull final Chunk chunk, @Nonnull final BlockPos pos) {
        return getFluidState(chunk, pos, chunk.getBlockState(pos));
    }

    /**
     * If the state at the position is a fluid, return it as a FluidState. Otherwise, return the FluidState at the position.
     * If you know that the state at the position is not a fluid, use {@link FluidState#getFromProvider(ICapabilityProvider, BlockPos) FluidState::getFromProvider} instead.
     *
     * @param chunk Chunk.
     * @param pos Position.
     * @param state IBlockState at the position.
     * @return The state at the position as a fluidState (if the state is a fluid). Otherwise, returns the fluidState at the position.
     *
     * @throws NullPointerException if pos is null.
     * @since 3.0.0
     */
    @Nonnull
    public static FluidState getFluidState(@Nullable final Chunk chunk, @Nonnull final BlockPos pos, @Nullable final IBlockState state) {
        return isFluid(state) ? FluidState.of(state) : FluidState.getFromProvider(chunk, pos);
    }

    /**
     *
     *
     * @param world IBlockAccess.
     * @param pos Position.
     * @return
     *
     * @throws NullPointerException if world or pos are null.
     * @since 1.7.0
     */
    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
        @Nullable final Chunk chunk = world instanceof IChunkProvider ? ((IChunkProvider)world).getChunk(pos) : null;
        return chunk != null ? getFluidOrReal(chunk, pos) : getFluidOrReal(world, pos, world.getBlockState(pos));
    }

    //tries to get the fluid at the pos (prioritizing ones physically in the world, then the fluid capability),
    //if none return input state
    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull final IBlockState state) {
        if(isFluid(state)) return state; // if the state here is a fluid, return it

        @Nonnull final FluidState fluidState = FluidState.get(world, pos);
        return fluidState.isEmpty() ? state : fluidState.getState();
    }

    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull final Chunk chunk, @Nonnull final BlockPos pos) {
        return getFluidOrReal(chunk, pos, chunk.getBlockState(pos));
    }

    @Nonnull
    public static IBlockState getFluidOrReal(@Nullable final Chunk chunk, @Nonnull final BlockPos pos, @Nonnull final IBlockState state) {
        if(isFluid(state)) return state; // if the state here is a fluid, return it

        @Nonnull final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
        return fluidState.isEmpty() ? state : fluidState.getState();
    }

    //convenience method that uses default block flags
    public static boolean setFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nullable IBlockState here, @Nonnull FluidState fluidState, boolean checkVaporize) {
        return setFluidState(world, pos, here, fluidState, checkVaporize, Constants.BlockFlags.DEFAULT);
    }

    public static boolean setFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nullable IBlockState here, @Nonnull FluidState fluidState, boolean checkVaporize, int blockFlags) {
        if(world.isOutsideBuildHeight(pos) || world.getWorldType() == WorldType.DEBUG_ALL_BLOCK_STATES) return false;

        @Nonnull final Chunk chunk = world.getChunk(pos);
        if(here == null) here = chunk.getBlockState(pos);
        if(fluidState != FluidState.EMPTY && isFluid(here)) return false;

        @Nullable final IFluidStateCapability cap = IFluidStateCapability.get(chunk);
        if(cap == null) throw new NullPointerException("There was a critical internal error involving the Fluidlogged API mod, notify the mod author!");
        else if(cap.getContainer(pos.getY()).getFluidState(pos, FluidState.EMPTY) == fluidState) return false;
        pos = pos.toImmutable();

        //update the chunk's precipitationHeightMap
        final int precipitationIndex = (pos.getZ() & 15) << 4 | (pos.getX() & 15);
        if(pos.getY() >= chunk.precipitationHeightMap[precipitationIndex] - 1) chunk.precipitationHeightMap[precipitationIndex] = -999;

        //handle event
        final FluidloggedEvent event = new FluidloggedEvent(world, chunk, pos, here, fluidState, checkVaporize, blockFlags);
        if(MinecraftForge.EVENT_BUS.post(event) && event.getResult() != Event.Result.DEFAULT) return event.getResult() == Event.Result.ALLOW;

        //if the world is too warm for the fluid, vaporize it
        if(event.doesVaporize()) {
            playVaporizeEffects(world, pos, event.getFluidStack());
            return true;
        }

        //check for IFluidloggable
        if(here.getBlock() instanceof IFluidloggable) {
            final EnumActionResult result = ((IFluidloggable)here.getBlock()).onFluidChange(world, pos, here, event.fluidState, event.blockFlags);
            if(result != EnumActionResult.PASS) return result == EnumActionResult.SUCCESS;
        }

        //moved to separate function, as to allow easy calling by IFluidloggable instances that use IFluidloggable#onFluidChange
        setFluidState_Internal(world, chunk, here, pos, event.fluidState, event.blockFlags);

        //default
        return true;
    }

    //if you're not an event instance or an IFluidloggable instance, use setFluidState instead!
    //moved to separate function, as to allow easy calling by IFluidloggable instances that use IFluidloggable#onFluidChange
    public static void setFluidState_Internal(@Nonnull World world, @Nonnull Chunk chunk, @Nonnull IBlockState here, @Nonnull BlockPos pos, @Nonnull FluidState fluidState, int blockFlags) {
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
        if(cap == null) throw new NullPointerException("There was a critical internal error involving the Fluidlogged API mod, notify the mod author!");
        else if(world.isRemote) { if(!cap.getContainer(pos.getY()).setFluidState(pos, fluidState)) return; }
        else {
            if(!cap.getContainer(pos.getY()).setFluidState(pos, fluidState)) return;
            else if((blockFlags & Constants.BlockFlags.SEND_TO_CLIENTS) != 0) // send changes to clients
                MessageUtils.sendToAllTracking(new SMessageSyncFluidState(pos, fluidState, true), chunk, FluidloggedAPI.WRAPPER);

            fluidState.getBlock().onBlockAdded(world, pos, fluidState.getState());
        }

        //update blocks & fluids
        relightFluidBlock(world, pos, chunk, fluidState);
        world.markAndNotifyBlock(pos, chunk, here, here, blockFlags);
    }

    //causes a light level & light opacity update
    public static void relightFluidBlock(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidState fluidState) {
        relightFluidBlock(world, pos, world.getChunk(pos), fluidState);
    }

    public static void relightFluidBlock(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final Chunk chunk, @Nonnull final FluidState fluidState) {
        final int x = pos.getX() & 15;
        final int z = pos.getZ() & 15;
        final int height = chunk.getHeightValue(x, z);

        if(!fluidState.isEmpty() && fluidState.getState().getLightOpacity(world, pos) > 0) {
            if(pos.getY() >= height) chunk.relightBlock(x, pos.getY() + 1, z);
        }

        else if(pos.getY() == height - 1) chunk.relightBlock(x, pos.getY(), z);
        if(chunk.getLightFor(EnumSkyBlock.SKY, pos) > 0 || chunk.getLightFor(EnumSkyBlock.BLOCK, pos) > 0) {
            chunk.propagateSkylightOcclusion(x, z);
        }

        world.profiler.startSection("checkLight");
        world.checkLight(pos);
        world.profiler.endSection();
    }

    public static boolean setFluidToAir(@Nonnull final World world, @Nonnull final BlockPos pos, @Nullable final IBlockState here, final int blockFlags) {
        if(here != null) return isFluid(here) ? world.setBlockState(pos, BlockStateContainer.AIR_BLOCK_STATE, blockFlags | 32) : setFluidState(world, pos, here, FluidState.EMPTY, false, blockFlags);

        @Nonnull final IBlockState state = world.getBlockState(pos);
        return isFluid(state) ? world.setBlockState(pos, BlockStateContainer.AIR_BLOCK_STATE, blockFlags | 32) : setFluidState(world, pos, state, FluidState.EMPTY, false, blockFlags);
    }

    //functions the same as World#notifyNeighborsOfStateChange, but for fluids
    public static void notifyFluids(@Nonnull World world, @Nonnull BlockPos pos, @Nullable FluidState fluidState, boolean notifyHere, @Nullable EnumFacing... except) {
        @Nonnull final FluidCache cache = new FluidCache(world, pos, 1, 1);
        @Nonnull final EnumSet<EnumFacing> set = EnumSet.allOf(EnumFacing.class);

        @Nonnull final IBlockState source = (fluidState == null ? cache.getFluidState(pos) : fluidState).getState();
        if(except != null) Arrays.asList(except).forEach(set::remove);
        if(ForgeEventFactory.onNeighborNotify(world, pos, source, set, false).isCanceled())
            return;

        //update state here
        if(notifyHere) source.neighborChanged(world, pos, source.getBlock(), pos);

        //update neighboring states
        for(@Nonnull final EnumFacing facing : set) {
            @Nonnull final BlockPos offset = pos.offset(facing);
            @Nonnull final FluidState neighbor = getFluidState(cache, offset);

            neighbor.getState().neighborChanged(world, offset, source.getBlock(), pos);
        }
    }

    //has two purposes:
    //1: returns true if the contained fluid can flow from the specified side
    //2: returns true if a fluid can flow into this block from the specified side
    public static boolean canFluidFlow(@Nonnull IBlockAccess access, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull EnumFacing side) {
        pos = pos.toImmutable(); // this is dumb, but without it corner rendering breaks for some reason

        // config override
        @Nonnull final IBlockState actualState = here.getActualState(access, pos);
        @Nullable final ICanFluidFlowHandler override = ICanFluidFlowHandler.Accessor.getOverride(actualState);
        if(override != null) return override.canFluidFlow(access, pos, actualState, side);

        // built-in behavior
        else return (here.getBlock() instanceof IFluidloggable)
                ? ((IFluidloggable)here.getBlock()).canFluidFlow(access, pos, here, side)
                : here.getBlockFaceShape(access, pos, side) != BlockFaceShape.SOLID;
    }

    //checks if two fluids are compatible
    public static boolean isCompatibleFluid(@Nullable Fluid fluid1, @Nullable Fluid fluid2) {
        if(fluid1 == null || fluid2 == null) return false;
        else if(fluid1.equals(fluid2)) return true;

        final int compat1 = fluid1 instanceof ICompatibleFluid ? ((ICompatibleFluid)fluid1).getFluidCompatibility(fluid2) : 0;
        final int compat2 = fluid2 instanceof ICompatibleFluid ? ((ICompatibleFluid)fluid2).getFluidCompatibility(fluid1) : 0;
        return (compat1 == compat2 ? compat1 : Math.max(compat1, compat2) - Math.min(compat1, compat2)) > 0;
    }

    /**
     * @since 3.0.0
     */
    public static boolean isCompatibleFluid(@Nonnull final FluidState fluidState1, @Nonnull final FluidState fluidState2) {
        return isCompatibleFluid(fluidState1.getFluid(), fluidState2.getFluid());
    }

    //convenience method that takes in an IBlockState rather than a Block
    @Nullable
    public static Fluid getFluidFromState(@Nullable IBlockState fluid) {
        if(fluid == null) return null;
        else if(fluid.getBlock() instanceof IFluidBlock) return ((IFluidBlock)fluid.getBlock()).getFluid();

        final Material material = fluid.getMaterial();
        if(material == Material.WATER) return FluidRegistry.WATER;
        else return material == Material.LAVA ? FluidRegistry.LAVA : null;
    }

    //fork of IFluidBlock#getFluid
    //(BlockLiquid extends IFluidBlock during runtime through asm)
    @Nullable
    public static Fluid getFluidFromBlock(@Nullable Block fluid) {
        if(fluid instanceof IFluidBlock) return ((IFluidBlock)fluid).getFluid();
        else if(fluid == null) return null;

        @Nonnull final Material material = fluid.getDefaultState().getMaterial();
        return material == Material.WATER ? FluidRegistry.WATER : material == Material.LAVA ? FluidRegistry.LAVA : null;
    }

    //return true if the input state or block is a fluid
    public static boolean isFluid(@Nullable Block fluid) {
        if(fluid instanceof IFluidBlock) return true;
        else if(fluid == null) return false;

        @Nonnull final Material material = fluid.getDefaultState().getMaterial();
        return material == Material.WATER || material == Material.LAVA;
    }

    public static boolean isFluid(@Nullable IBlockState fluid) {
        if(fluid == null) return false;
        else if(fluid.getBlock() instanceof IFluidBlock) return true;

        @Nonnull final Material material = fluid.getMaterial();
        return material == Material.WATER || material == Material.LAVA;
    }

    /**
     * Deprecated since 3.0.0, use {@link FluidState#getFluidBlockHandler()} instead.
     *
     * @since 1.8.0
     */
    @Deprecated
    public static boolean isFluidloggableFluid(@Nonnull IBlockState fluid, @Nonnull World world, @Nonnull BlockPos pos) {
        @Nonnull final FluidState fluidState = FluidState.of(fluid);
        return fluidState.isFluidloggable() && fluidState.getFluidBlockHandler().isFluidloggableFluid(fluidState);
    }

    /**
     * @param state
     * @param world
     * @param pos
     * @param fluid
     * @return
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     */
    public static boolean isStateFluidloggable(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluid) {
        return fluid.isFluidloggable() && fluid.getFluidBlockHandler().isFluidloggableFluid(fluid) && fluid.getFluidBlockHandler().isStateFluidloggable(state, world, pos, fluid);
    }

    public static boolean canCreateSource(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos) {
        return ForgeEventFactory.canCreateFluidSource(world, pos, state, state.getBlock() instanceof BlockLiquid ? state.getMaterial() == Material.WATER
                : state.getBlock() instanceof PluginBlockFluidClassic.Accessor && ((PluginBlockFluidClassic.Accessor)state.getBlock()).canCreateSource_Public());
    }

    public static boolean canFluidOccupy(@Nonnull final IBlockState actualState, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
        if(canFluidFlow(world, pos, actualState, fluidState.getDownDensityFace())) return true;
        else if(!fluidState.isValid()) return false;

        // estimate box occupied by the fluid
        @Nonnull final AxisAlignedBB fluidBB = fluidState.getFluidBox(world, pos);

        // allow configs to specify boxes for improperly coded blocks
        @Nonnull final IConfigFluidBox config = IConfigFluidBox.get(actualState);
        if(config.getBoxes() != null) return config.getBoxes().stream()
                .noneMatch(box -> box.max + pos.getY() >= fluidBB.maxY && box.min + pos.getY() <= fluidBB.minY);

        // gather the block's boxes, and remove any that always fail to block the fluid (the fluid can flow above or below them)
        @Nonnull final List<AxisAlignedBB> colliding = new ArrayList<>();
        actualState.addCollisionBoxToList(IWorldProvider.getWorld(world), pos, fluidBB, colliding, null, true);
        colliding.removeIf(bb -> bb.maxY < fluidBB.maxY || bb.minY > fluidBB.minY);

        // ----------------------------------------------
        // find any holes that the fluid can pass through
        // ----------------------------------------------

        if(colliding.isEmpty()) return true;
        @Nonnull final Rectangle2D[] trimmed = colliding.stream()
                .map(bb -> new Rectangle2D.Double(bb.minX, bb.minZ, bb.maxX - bb.minX, bb.maxZ - bb.minZ))
                .map(new Rectangle2D.Double(pos.getX(), pos.getZ(), 1, 1)::createIntersection)
                .toArray(Rectangle2D[]::new);

        double total = 0, overlapping = 0;
        for(int i = 0; i < trimmed.length; i++) {
            total += trimmed[i].getWidth() * trimmed[i].getHeight();
            for(int j = i + 1; j < trimmed.length; j++) {
                if(trimmed[i].intersects(trimmed[j])) {
                    @Nonnull final Rectangle2D overlap = trimmed[i].createIntersection(trimmed[j]);
                    overlapping += overlap.getWidth() * overlap.getHeight();
                }
            }
        }

        return total - overlapping < 1;
    }

    public static void playVaporizeEffects(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final FluidStack fluidStack) {
        if(!world.isRemote) {
            fluidStack.getFluid().vaporize(null, world, pos, fluidStack); // play serverside effects (like sounds)
            FluidloggedAPI.WRAPPER.sendToAllAround(new SMessageVaporizeEffects(fluidStack, pos),
                    new NetworkRegistry.TargetPoint(world.provider.getDimension(), pos.getX() + 0.5, pos.getY() + 0.5, pos.getZ() + 0.5, 64));
        }
    }
}
