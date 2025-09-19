/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
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
import net.minecraft.util.math.Vec3d;
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
import java.util.*;

/**
 * A utility class containing various functions for FluidStates.
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
     * @throws NullPointerException If world or pos are null.
     * @since 1.7.0
     * @author jbred
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
     * @throws NullPointerException If pos is null.
     * @since 1.7.0
     * @author jbred
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
     * @throws NullPointerException If chunk or pos are null.
     * @since 3.0.0
     * @author jbred
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
     * @throws NullPointerException If pos is null.
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public static FluidState getFluidState(@Nullable final Chunk chunk, @Nonnull final BlockPos pos, @Nullable final IBlockState state) {
        return isFluid(state) ? FluidState.of(state) : FluidState.getFromProvider(chunk, pos);
    }

    /**
     * @param world IBlockAccess.
     * @param pos Position.
     * @return The IBlockState at the position if it's either a fluid or if there's no FluidState at the position. Otherwise, return the FluidState at the position.
     *
     * @throws NullPointerException If chunk or pos are null.
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
        @Nullable final Chunk chunk = world instanceof IChunkProvider ? ((IChunkProvider)world).getChunk(pos) : null;
        return chunk != null ? getFluidOrReal(chunk, pos) : getFluidOrReal(world, pos, world.getBlockState(pos));
    }

    /**
     * @param world IBlockAccess.
     * @param pos Position.
     * @param state IBlockState at the position.
     * @return The IBlockState at the position if it's either a fluid or if there's no FluidState at the position. Otherwise, return the FluidState at the position.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state) {
        if(isFluid(state)) return state; // if the state here is a fluid, return it

        @Nonnull final FluidState fluidState = FluidState.get(world, pos);
        return fluidState.isEmpty() ? state : fluidState.getState();
    }

    /**
     * @param chunk Chunk.
     * @param pos Position.
     * @return The IBlockState at the position if it's either a fluid or if there's no FluidState at the position. Otherwise, return the FluidState at the position.
     *
     * @throws NullPointerException If chunk or pos are null.
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public static IBlockState getFluidOrReal(@Nonnull final Chunk chunk, @Nonnull final BlockPos pos) {
        return getFluidOrReal(chunk, pos, chunk.getBlockState(pos));
    }

    /**
     * @param chunk Chunk.
     * @param pos Position.
     * @param state IBlockState at the position.
     * @return The IBlockState at the position if it's either a fluid or if there's no FluidState at the position. Otherwise, return the FluidState at the position.
     *
     * @throws NullPointerException If pos or state are null.
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public static IBlockState getFluidOrReal(@Nullable final Chunk chunk, @Nonnull final BlockPos pos, @Nonnull final IBlockState state) {
        if(isFluid(state)) return state; // if the state here is a fluid, return it

        @Nonnull final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
        return fluidState.isEmpty() ? state : fluidState.getState();
    }

    /**
     * Only if the block state here is a fluid block, this sets it to air. Otherwise, this sets the FluidState at the position to {@link FluidState#EMPTY}.
     *
     * @param world
     * @param pos
     * @param here
     * @param blockFlags
     * @return True if a FluidState or fluid block were set to air, false otherwise.
     *
     * @throws NullPointerException If world or pos are null.
     * @since 3.0.0
     * @author jbred
     */
    public static boolean setFluidToAir(@Nonnull final World world, @Nonnull final BlockPos pos, @Nullable final IBlockState here, final int blockFlags) {
        if(here != null) return isFluid(here) ? world.setBlockState(pos, BlockStateContainer.AIR_BLOCK_STATE, blockFlags | 32) : setFluidState(world, pos, here, FluidState.EMPTY, false, blockFlags);

        @Nonnull final IBlockState state = world.getBlockState(pos);
        return isFluid(state) ? world.setBlockState(pos, BlockStateContainer.AIR_BLOCK_STATE, blockFlags | 32) : setFluidState(world, pos, state, FluidState.EMPTY, false, blockFlags);
    }

    /**
     * Set a FluidState in the world.
     *
     * @param world World.
     * @param pos Position.
     * @param here IBlockState at the position, may be null.
     * @param fluidState FluidState to place in the world, may be empty.
     * @param checkVaporize True if fluid vaporizing should be checked. For example, water vaporizing in the nether.
     * @return True if the FluidState was successfully set, false otherwise.
     *
     * @throws NullPointerException If world, pos, or fluidState are null.
     * @since 1.7.0
     * @author jbred
     */
    public static boolean setFluidState(@Nonnull final World world, @Nonnull final BlockPos pos, @Nullable final IBlockState here, @Nonnull final FluidState fluidState, final boolean checkVaporize) {
        return setFluidState(world, pos, here, fluidState, checkVaporize, Constants.BlockFlags.DEFAULT);
    }

    /**
     * Set a FluidState in the world.
     *
     * @param world World.
     * @param pos Position.
     * @param here IBlockState at the position, may be null.
     * @param fluidState FluidState to place in the world, may be empty.
     * @param checkVaporize True if fluid vaporizing should be checked. For example, water vaporizing in the nether.
     * @param blockFlags Flag 1 will cause a block update. Flag 2 will send the change to clients. Flag 4 will prevent the block from
     * being re-rendered, if this is a client world. Flag 8 will force any re-renders to run on the main thread instead
     * of the worker pool, if this is a client world and flag 4 is clear. Flag 16 will prevent observers from seeing
     * this change. Flags can be OR-ed.
     * @return True if the FluidState was successfully set, false otherwise.
     *
     * @throws NullPointerException If world, pos, or fluidState are null.
     * @since 1.7.0
     * @author jbred
     */
    public static boolean setFluidState(@Nonnull final World world, @Nonnull BlockPos pos, @Nullable IBlockState here, @Nonnull final FluidState fluidState, final boolean checkVaporize, final int blockFlags) {
        if(world.isOutsideBuildHeight(pos) || world.getWorldType() == WorldType.DEBUG_ALL_BLOCK_STATES) return false;

        @Nonnull final Chunk chunk = world.getChunk(pos);
        if(here == null) here = chunk.getBlockState(pos);
        if(fluidState != FluidState.EMPTY && isFluid(here)) return false;

        @Nullable final IFluidStateCapability cap = IFluidStateCapability.get(chunk);
        if(cap == null) throw new NullPointerException("There was a critical internal error involving the Fluidlogged API mod, notify the mod author!");
        else if(cap.getContainer(pos.getY()).getFluidState(pos, FluidState.EMPTY) == fluidState) return false;
        pos = pos.toImmutable();

        // update the chunk's precipitationHeightMap
        final int precipitationIndex = (pos.getZ() & 15) << 4 | (pos.getX() & 15);
        if(pos.getY() >= chunk.precipitationHeightMap[precipitationIndex] - 1) chunk.precipitationHeightMap[precipitationIndex] = -999;

        // handle event
        final FluidloggedEvent event = new FluidloggedEvent(world, chunk, pos, here, fluidState, checkVaporize, blockFlags);
        if(MinecraftForge.EVENT_BUS.post(event) && event.getResult() != Event.Result.DEFAULT) return event.getResult() == Event.Result.ALLOW;

        // if the world is too warm for the fluid, vaporize it
        if(event.doesVaporize()) {
            playVaporizeEffects(world, pos, event.getFluidStack());
            return true;
        }

        // check for IFluidloggable
        if(here.getBlock() instanceof IFluidloggable) {
            final EnumActionResult result = ((IFluidloggable)here.getBlock()).onFluidChange(world, pos, here, event.fluidState, event.blockFlags);
            if(result != EnumActionResult.PASS) return result == EnumActionResult.SUCCESS;
        }

        // moved to separate function, as to allow easy calling by IFluidloggable instances that use IFluidloggable#onFluidChange
        setFluidState_Internal(world, chunk, here, pos, event.fluidState, event.blockFlags);

        // default
        return true;
    }

    // if you're not an event instance or an IFluidloggable instance, use setFluidState instead!
    // moved to separate function, as to allow easy calling by IFluidloggable instances that use IFluidloggable#onFluidChange
    public static void setFluidState_Internal(@Nonnull final World world, @Nonnull final Chunk chunk, @Nonnull final IBlockState here, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState, final int blockFlags) {
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
        if(cap == null) throw new NullPointerException("There was a critical internal error involving the Fluidlogged API mod, notify the mod author!");
        else if(world.isRemote) { if(!cap.getContainer(pos.getY()).setFluidState(pos, fluidState)) return; }
        else {
            if(!cap.getContainer(pos.getY()).setFluidState(pos, fluidState)) return;
            else if((blockFlags & Constants.BlockFlags.SEND_TO_CLIENTS) != 0) // send changes to clients
                MessageUtils.sendToAllTracking(new SMessageSyncFluidState(pos, fluidState, true), chunk, FluidloggedAPI.WRAPPER);

            fluidState.getBlock().onBlockAdded(world, pos, fluidState.getState());
        }

        // update blocks & fluids
        relightFluidBlock(world, pos, chunk, fluidState);
        world.markAndNotifyBlock(pos, chunk, here, here, blockFlags);
    }

    /**
     * Causes a light level & light opacity update.
     * @param world World.
     * @param pos Position.
     * @param fluidState FluidState causing the update, may be empty.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 1.7.0
     * @author jbred
     */
    public static void relightFluidBlock(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
        relightFluidBlock(world, pos, world.getChunk(pos), fluidState);
    }

    /**
     * Causes a light level & light opacity update.
     * @param world World.
     * @param pos Position.
     * @param chunk Chunk.
     * @param fluidState FluidState causing the update, may be empty.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     * @author jbred
     */
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

    /**
     * The same as {@link World#notifyNeighborsOfStateChange(BlockPos, Block, boolean)}, but for fluids.
     * @param world World.
     * @param pos Position.
     * @param fluidState FluidState causing the update. If null, it's assumed that the FluidState at the position caused the update.
     * @param notifyHere True if the FluidState at the position should be notified.
     * @param except Sides to not send updates.
     *
     * @throws NullPointerException If world or pos are null.
     * @since 1.7.0
     * @author jbred
     */
    public static void notifyFluids(@Nonnull final World world, @Nonnull final BlockPos pos, @Nullable final FluidState fluidState, final boolean notifyHere, @Nullable final EnumFacing... except) {
        @Nonnull final FluidCache cache = new FluidCache(world, pos, 1, 1);
        @Nonnull final EnumSet<EnumFacing> set = EnumSet.allOf(EnumFacing.class);

        @Nonnull final IBlockState source = (fluidState == null ? cache.getFluidState(pos) : fluidState).getState();
        if(except != null) Arrays.asList(except).forEach(set::remove);
        if(ForgeEventFactory.onNeighborNotify(world, pos, source, set, false).isCanceled())
            return;

        // update state here
        if(notifyHere) source.neighborChanged(world, pos, source.getBlock(), pos);

        // update neighboring states
        for(@Nonnull final EnumFacing facing : set) {
            @Nonnull final BlockPos offset = pos.offset(facing);
            @Nonnull final FluidState neighbor = getFluidState(cache, offset);

            neighbor.getState().neighborChanged(world, offset, source.getBlock(), pos);
        }
    }

    /**
     * <li> Returns true if the contained fluid can flow from the specified side.</li>
     * <li> Returns true if a fluid can flow into this block from the specified side.</li>
     *
     * @param access IBlockAccess.
     * @param pos Position.
     * @param here IBlockState at the position.
     * @param side Side to test.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 1.7.0
     * @author jbred
     */
    public static boolean canFluidFlow(@Nonnull final IBlockAccess access, @Nonnull BlockPos pos, @Nonnull final IBlockState here, @Nonnull final EnumFacing side) {
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

    /**
     * @param fluid1 Fluid.
     * @param fluid2 Fluid to be compared to fluid1.
     * @return True if fluid1 and fluid2 are compatible. See {@link ICompatibleFluid}.
     *
     * @since 1.7.0
     * @author jbred
     */
    public static boolean isCompatibleFluid(@Nullable final Fluid fluid1, @Nullable final Fluid fluid2) {
        if(fluid1 == null || fluid2 == null) return false;
        else if(fluid1.equals(fluid2)) return true;

        final int compat1 = fluid1 instanceof ICompatibleFluid ? ((ICompatibleFluid)fluid1).getFluidCompatibility(fluid2) : 0;
        final int compat2 = fluid2 instanceof ICompatibleFluid ? ((ICompatibleFluid)fluid2).getFluidCompatibility(fluid1) : 0;
        return (compat1 == compat2 ? compat1 : Math.max(compat1, compat2) - Math.min(compat1, compat2)) > 0;
    }

    /**
     * @param fluidState1 FluidState.
     * @param fluidState2 FluidState whose fluid is to be compared with the fluid of fluidState1.
     * @return True if the fluids of fluidState1 and fluidState2 are compatible. See {@link ICompatibleFluid}.
     *
     * @throws NullPointerException If fluidState1 or fluidState2 are null.
     * @since 3.0.0
     * @author jbred
     */
    public static boolean isCompatibleFluid(@Nonnull final FluidState fluidState1, @Nonnull final FluidState fluidState2) {
        return isCompatibleFluid(fluidState1.getFluid(), fluidState2.getFluid());
    }

    /**
     * @param state IBlockState.
     * @return The fluid associated with the provided block state, or null if the provided block state is not a fluid block.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nullable
    public static Fluid getFluidFromState(@Nullable final IBlockState state) {
        if(state == null) return null;
        else if(state.getBlock() instanceof IFluidBlock) return ((IFluidBlock)state.getBlock()).getFluid();

        @Nonnull final Material material = state.getMaterial();
        return material == Material.WATER ? FluidRegistry.WATER : material == Material.LAVA ? FluidRegistry.LAVA : null;
    }

    /**
     * @param block Block.
     * @return The fluid associated with the provided block, or null if the provided block is not a fluid block.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nullable
    public static Fluid getFluidFromBlock(@Nullable final Block block) {
        if(block instanceof IFluidBlock) return ((IFluidBlock)block).getFluid();
        else if(block == null) return null;

        @Nonnull final Material material = block.getDefaultState().getMaterial();
        return material == Material.WATER ? FluidRegistry.WATER : material == Material.LAVA ? FluidRegistry.LAVA : null;
    }

    /**
     * @param state IBlockState.
     * @return True if the input block state is a fluid.
     *
     * @since 1.9.0
     * @author jbred
     */
    public static boolean isFluid(@Nullable final IBlockState state) {
        if(state == null) return false;
        else if(state.getBlock() instanceof IFluidBlock) return true;

        @Nonnull final Material material = state.getMaterial();
        return material == Material.WATER || material == Material.LAVA;
    }

    /**
     * @param block Block.
     * @return True if the input block is a fluid.
     *
     * @since 1.9.0
     * @author jbred
     */
    public static boolean isFluid(@Nullable final Block block) {
        if(block instanceof IFluidBlock) return true;
        else if(block == null) return false;

        @Nonnull final Material material = block.getDefaultState().getMaterial();
        return material == Material.WATER || material == Material.LAVA;
    }

    /**
     * @param state IBlockState to check.
     * @param world IBlockAccess.
     * @param pos Position.
     * @param fluid FluidState to check.
     * @return True if the provided block state can be fluidlogged with the provided FluidState.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     * @author jbred
     */
    public static boolean isStateFluidloggable(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluid) {
        return fluid.isFluidloggable() && fluid.getFluidBlockHandler().isStateFluidloggable(state, world, pos, fluid);
    }

    /**
     * @param state The fluid block as a block state.
     * @param world World.
     * @param pos Position.
     * @return True if the fluid block state can create source blocks.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     * @author jbred
     */
    public static boolean canCreateSource(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos) {
        return ForgeEventFactory.canCreateFluidSource(world, pos, state, state.getBlock() instanceof BlockLiquid ? state.getMaterial() == Material.WATER
                : state.getBlock() instanceof PluginBlockFluidClassic.Accessor && ((PluginBlockFluidClassic.Accessor)state.getBlock()).canCreateSource_Public());
    }

    /**<b>
     * This method is intended to only be used by {@link IFluidloggable#isFluidloggable(IBlockState, IBlockAccess, BlockPos, FluidState) IFluidloggable.isFluidloggable()},
     * and is completely separate from normal fluidlogging checks.</b> For those, only use {@link FluidState#isFluidloggable()} and
     * {@link FluidloggedUtils#isStateFluidloggable(IBlockState, IBlockAccess, BlockPos, FluidState) isStateFluidloggable()}.
     * <p>
     * This method checks that the provided FluidState will be able to *fit outside* the provided actual state. It does this by comparing
     * the state's collision boxes (or user-specified "boxes" if provided) with the estimated bounding box for the FluidState, and
     * making sure that part of the FluidState's bounding box sticks out.
     * </p>
     *
     * @param actualState {@link IBlockState#getActualState Actual IBlockState} to test.
     * @param world IBlockAccess.
     * @param pos Position.
     * @param fluidState FluidState to test.
     * @return True if the FluidState fits outside the provided actual state.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     * @author jbred
     */
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

    /**
     * Utility method that runs {@link Fluid#vaporize} from the server, while still allowing clients to see any particles.
     *
     * @param world World.
     * @param pos Position.
     * @param fluidStack FluidStack.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.0.0
     * @author jbred
     */
    public static void playVaporizeEffects(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final FluidStack fluidStack) {
        if(!world.isRemote) {
            fluidStack.getFluid().vaporize(null, world, pos, fluidStack); // play serverside effects (like sounds)
            FluidloggedAPI.WRAPPER.sendToAllAround(new SMessageVaporizeEffects(fluidStack, pos),
                    new NetworkRegistry.TargetPoint(world.provider.getDimension(), pos.getX() + 0.5, pos.getY() + 0.5, pos.getZ() + 0.5, 64));
        }
    }

    /**
     * Utility method that returns the position to spawn a drip particle, or empty if one cannot be spawned.
     * A general use case looks like this:
     * <blockquote><pre>
     * &#64;Override
     * &#64;SideOnly(Side.CLIENT)
     * public void randomDisplayTick(IBlockState stateIn, World worldIn,
     *                               BlockPos pos, Random rand) {
     *     if(rand.nextInt(10) == 0) {
     *         FluidloggedUtils.positionDripParticle(worldIn, pos, FluidState.of(stateIn))
     *         .ifPresent(particlePos -> ...);
     *     }
     * }
     * </pre></blockquote>
     *
     * @param world World.
     * @param pos Position of the FluidState.
     * @param fluidState FluidState creating the drip particle.
     *
     * @throws NullPointerException If any of the parameters are null.
     * @since 3.1.0
     * @author jbred
     */
    @Nonnull
    public static Optional<Vec3d> positionDripParticle(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
        final int densityDir = fluidState.getDensityDir();
        @Nonnull final Chunk chunk = world.getChunk(pos);
        @Nonnull final IBlockState here = chunk.getBlockState(pos), below = chunk.getBlockState(pos.up(densityDir));

        // spawn drip particle under this
        if(here != fluidState.getState() && !canFluidFlow(world, pos, here, fluidState.getDownDensityFace())) {
            if(!below.getMaterial().blocksMovement() && getFluidState(chunk, pos.up(densityDir), below).isEmpty()) {
                return Optional.of(new Vec3d(pos).add(world.rand.nextDouble(), densityDir < 0 ? -0.05 : 1.05, world.rand.nextDouble()));
            }
        }

        // spawn drip particle under the block below this
        if(!canFluidFlow(world, pos.up(densityDir), below, fluidState.getUpDensityFace()) && getFluidState(chunk, pos.up(densityDir), below).isEmpty()) {
            @Nonnull final IBlockState under = chunk.getBlockState(pos.up(densityDir << 1));
            if(!under.getMaterial().blocksMovement() && getFluidState(chunk, pos.up(densityDir << 1), under).isEmpty()) {
                return Optional.of(new Vec3d(pos).add(world.rand.nextDouble(), densityDir < 0 ? -1.05 : 2.05, world.rand.nextDouble()));
            }
        }

        // cannot spawn drip particle
        return Optional.empty();
    }

    /**
     * Deprecated since 3.0.0, use {@link FluidState#isFluidloggable()} instead.
     *
     * @since 1.8.0
     * @author jbred
     */
    @Deprecated
    public static boolean isFluidloggableFluid(@Nullable final Block fluid) {
        return FluidState.of(fluid).isFluidloggable();
    }

    /**
     * Deprecated since 3.0.0, use {@link FluidState#isFluidloggable()} instead.
     *
     * @since 1.8.0
     * @author jbred
     */
    @Deprecated
    public static boolean isFluidloggableFluid(@Nullable final IBlockState fluid, @Nullable final World world, @Nullable final BlockPos pos) {
        return FluidState.of(fluid).isFluidloggable();
    }

    /**
     * Deprecated since 3.0.0, use the FluidState-sensitive version instead.
     *
     * @throws NullPointerException If state, world, or pos are null.
     * @since 1.8.0
     * @author jbred
     */
    @Deprecated
    public static boolean isStateFluidloggable(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos, @Nullable final Fluid fluid) {
        return isStateFluidloggable(state, world, pos, FluidState.of(fluid));
    }
}
