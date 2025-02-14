/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.util;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.fluid.IFlowCostFluid;
import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.world.IFluidStateProvider;
import git.jbredwards.fluidlogged_api.api.world.IWorldProvider;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IDefaultFluidState;
import git.jbredwards.fluidlogged_api.mod.asm.iface.ILevelFluidStateLookup;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginBlockFluidBase;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import net.minecraft.block.Block;
import net.minecraft.block.BlockDynamicLiquid;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.BlockStaticLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.BlockStateContainer;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.Map;

/**
 * holds a fluid & a block state
 *
 * @since 1.7.0
 * @author jbred
 *
 */
@Immutable
public class FluidState extends Pair<Fluid, IBlockState> implements Object2ObjectMap.Entry<Fluid, IBlockState>
{
    /**
     * This exists to prevent undesired FluidStates from appearing during world gen (issue#189). During world generation:
     * this is set to true, however it is false at all other times (cannot ever be null). This functions similarly to
     * {@link net.minecraft.block.BlockFalling#fallInstantly BlockFalling.fallInstantly}. Notes:
     * <ul>
     * <li>This exists to fix older (unsupported) mods. New mods not wanting to keep FluidStates on blockState
     * placement should use the 32 flag (x | 32, example: Constants.BlockFlags.SEND_TO_CLIENT | 32)
     * when calling {@link World#setBlockState(BlockPos, IBlockState, int)}. Likewise, new mods can use the 64 flag to
     * skip this check and <i>keep</i> any existing FluidStates.</li>
     * <li>Blocks that implement {@link git.jbredwards.fluidlogged_api.api.block.IFluidloggable IFluidloggable} are
     * exempt from this check, and will always keep existing FluidStates on world gen (unless the 32 flag is set,
     * in which case any existing FluidState is removed).</li>
     * <li>For mods using a {@link net.minecraft.world.gen.structure.template.Template Template-based} generation system,
     * and that want to <i>keep</i> existing FluidStates, you can apply the 64 flag, or use an nbt editor program to
     * set the "keepFluidStates" flag to true (false by default) in the structure's .nbt file.</li>
     * </ul>
     * @since 3.0.0
     */
    @Nonnull
    public static final ThreadLocal<Boolean> removeOnBlockChange = ThreadLocal.withInitial(() -> Boolean.FALSE);

    /**
     * Always used instead of a null value.
     * @since 1.7.0
     */
    @Nonnull
    public static final FluidState EMPTY = new EmptyFluidState();

    protected final Fluid fluid;
    protected final IBlockState state;
    protected byte level = -1, meta = -1; // cached for better performance
    protected int propsKey = -1; // a generated number used to quickly look up a FluidState with a desired level

    protected FluidState(@Nullable final Fluid fluidIn, final IBlockState stateIn) {
        fluid = fluidIn;
        state = stateIn; // shouldn't ever actually be null
    }

    /**
     * @param fluid Fluid.
     * @return The cached FluidState stored in the fluid block's default state. If it has no fluid state or if
     * the fluid simply has no block associated with it, return {@link FluidState#EMPTY}. If it has no cached fluid
     * state, this: creates a new fluid state, caches it in the fluid block's default state, and returns it.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public static FluidState of(@Nullable final Fluid fluid) { return fluid != null ? of(fluid.getBlock()) : EMPTY; }

    /**
     * @param block Block.
     * @return The cached FluidState stored in the block's default state. If it has no fluid state
     * and is not a fluid block, return {@link FluidState#EMPTY}. If it's a fluid block and
     * has no cached fluid state, this: creates a new fluid state, caches it in the block's default state, and returns it.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public static FluidState of(@Nullable final Block block) { return block != null ? of(block.getDefaultState()) : EMPTY; }

    /**
     * @param stateIn IBlockState.
     * @return The cached FluidState stored in stateIn. If stateIn has no fluid state
     * and is not a fluid block, return {@link FluidState#EMPTY}. If stateIn is a fluid block and
     * has no cached fluid state, this: creates a new fluid state, caches it in stateIn, and returns it.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public static FluidState of(@Nullable final IBlockState stateIn) {
        if(stateIn == null) return EMPTY;
        // scrub any extra properties from the block state, to prevent possible duplicate FluidState instances during runtime
        @Nonnull final IBlockState state = stateIn instanceof IExtendedBlockState ? ((IExtendedBlockState)stateIn).getClean() : stateIn;

        // use the default state if present
        @Nullable final FluidState defaultFluidState = ((IDefaultFluidState)state).getDefaultFluidState();
        if(defaultFluidState != null) return defaultFluidState;

        // use empty if the input state is not a fluid
        @Nullable final Fluid fluid = FluidloggedUtils.getFluidFromState(state);
        if(fluid == null) {
            ((IDefaultFluidState)state).setDefaultFluidState(EMPTY);
            return EMPTY;
        }

        // set a default FluidState for later use
        @Nonnull final FluidState fluidState = fluid.getBlock() != state.getBlock()
                && !(fluid.getBlock() instanceof BlockLiquid) && fluid.getBlock() instanceof IFluidBlock
                ? of(fluid).withLevel(state.getValue(BlockLiquid.LEVEL)) : new FluidState(fluid, state);
        ((IDefaultFluidState)state).setDefaultFluidState(fluidState);
        return fluidState;
    }

    /**
     * @param world IBlockAccess instance, expected to implement {@link IFluidStateProvider}.
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @return The fluid state stored at the position, if none this returns {@link FluidState#EMPTY}.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public static FluidState get(@Nullable final IBlockAccess world, final int x, final int y, final int z) {
        return world instanceof IFluidStateProvider ? ((IFluidStateProvider)world).getFluidState(x, y, z) : EMPTY;
    }

    /**
     * @param world IBlockAccess instance, expected to implement {@link IFluidStateProvider}.
     * @param pos Position.
     * @return The fluid state stored at the position, if none this returns {@link FluidState#EMPTY}.
     *
     * @throws NullPointerException If pos is null.
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public static FluidState get(@Nullable final IBlockAccess world, @Nonnull final BlockPos pos) {
        return get(world, pos.getX(), pos.getY(), pos.getZ());
    }

    /**
     * @param pos Position.
     * @return The fluid state stored at the position from the {@link net.minecraft.client.Minecraft#world client world} instance,
     * if no fluid state is here this returns {@link FluidState#EMPTY}.
     *
     * @throws NullPointerException If pos is null.
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    @SideOnly(Side.CLIENT)
    public static FluidState get(@Nonnull final BlockPos pos) { return get(IWorldProvider.getClientWorld(), pos); }

    /**
     * @param provider Capability provider (usually a chunk). If the capability provider is a World,
     *                 you should call {@link FluidState#get(IBlockAccess, BlockPos) FluidState::get} instead.
     * @param x X position.
     * @param y Y position.
     * @param z Z position.
     * @return The fluid state stored at the position, if none this returns {@link FluidState#EMPTY}.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public static FluidState getFromProvider(@Nullable final ICapabilityProvider provider, final int x, final int y, final int z) {
        @Nullable final IFluidStateCapability cap = IFluidStateCapability.get(provider);
        return cap == null ? EMPTY : cap.getContainer(y).getFluidState(x, y, z, EMPTY);
    }

    /**
     * @param provider Capability provider (usually a chunk). If the capability provider is a World,
     *                 you should call {@link FluidState#get(IBlockAccess, BlockPos) FluidState::get} instead.
     * @param pos Position.
     * @return The fluid state stored at the position, if none this returns {@link FluidState#EMPTY}.
     *
     * @throws NullPointerException If pos is null.
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public static FluidState getFromProvider(@Nullable final ICapabilityProvider provider, @Nonnull final BlockPos pos) {
        return getFromProvider(provider, pos.getX(), pos.getY(), pos.getZ());
    }

    /**
     * @param serialized The serialized FluidState.
     * @return The deserialized FluidState, or {@link FluidState#EMPTY} if none was found.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public static FluidState deserialize(final int serialized) { return of(Block.BLOCK_STATE_IDS.getByValue(serialized)); }

    /**
     * @return This FluidState serialized as an int, which can be deserialized at a later time by calling
     * {@link FluidState#deserialize(int)}.
     *
     * @since 1.7.0
     * @author jbred
     */
    public int serialize() { return Block.BLOCK_STATE_IDS.get(getState()); }

    /**
     * @return True if this FluidState does not contain a fluid.
     *
     * @since 1.7.0
     * @author jbred
     */
    public boolean isEmpty() { return false; }

    /**
     * @return The FluidState's fluid, or null if this FluidState is empty.
     *
     * @since 1.7.0
     * @author jbred
     */
    public Fluid getFluid() { return fluid; }

    /**
     * @return The FluidState's block state, {@link FluidState#EMPTY} returns air's default state.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public IBlockState getState() { return state; }

    /**
     * @return The FluidState's block state metadata value.
     *
     * @since 3.0.0
     * @author jbred
     */
    public int getMetadata() { return meta != -1 ? meta : (meta = (byte)getBlock().getMetaFromState(getState())); }

    /**
     * @return True if this FluidState's block is an instance of {@link IFluidBlock}.
     *
     * @since 1.8.0
     * @author jbred
     */
    public final boolean isValid() { return getBlock() instanceof IFluidBlock; }

    /**
     * @return True if this FluidState's block can handle fluidlogging.
     *
     * @since 3.0.0
     * @author jbred
     */
    public final boolean isFluidloggable() { return getBlock() instanceof IFluidloggableFluid; }

    /**
     * Some FluidStates may contain badly coded fluid blocks that don't implement {@link IFluidBlock}.
     * It's advised to check {@link FluidState#isValid()} before calling this method.
     *
     * @return The FluidState's fluid block.
     * @throws ClassCastException If this fluid block does not implement {@link IFluidBlock}.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    public final IFluidBlock getFluidBlock() { return (IFluidBlock)getBlock(); }

    /**
     * Though I don't know of any, it's possible that some mod out there may have an entirely custom {@link IFluidBlock}
     * implementation while not having fluidlogged api support. It's advised to check {@link FluidState#isFluidloggable()}
     * before calling this method.
     *
     * @return The FluidState's fluidlogging handler.
     * @throws ClassCastException If this fluid block does not implement {@link IFluidloggableFluid}.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public final IFluidloggableFluid getFluidBlockHandler() { return (IFluidloggableFluid)getBlock(); }

    /**
     * @return The FluidState's block.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public Block getBlock() { return getState().getBlock(); }

    /**
     * @return The FluidState's block material.
     *
     * @since 1.7.0
     * @author jbred
     */
    @Nonnull
    public Material getMaterial() { return getState().getMaterial(); }

    /**
     * {@link FluidState#isEmpty() FluidState::isEmpty} should typically be checked at least once before this method.
     *
     * @return The FluidState's fluid level.
     * @throws UnsupportedOperationException If this FluidState is empty.
     *
     * @since 1.7.0
     * @author jbred
     */
    public int getLevel() { return level != -1 ? level : (level = getState().getValue(BlockLiquid.LEVEL).byteValue()); }

    /**
     * {@link FluidState#isEmpty() FluidState::isEmpty} should typically be checked at least once before this method.
     *
     * @param world World instance.
     * @return
     *
     * @throws NullPointerException If world is null.
     * @throws UnsupportedOperationException If this FluidState is empty.
     *
     * @since 3.0.0
     * @author jbred
     */
    public int getWrappedLevel(@Nonnull final World world) {
        return getBlock() instanceof BlockLiquid && getLevel() >= 8 ? getFlowCost(world) : getLevel();
    }

    /**
     * @return True if this FluidState is a source block (can be picked up by a bucket), false otherwise.
     *
     * @since 3.0.0
     * @author jbred
     */
    public boolean isSource() {
        return isValid() && getLevel() == (getBlock() instanceof BlockFluidFinite ? getQuantaPerBlock() - 1 : 0);
    }

    /**
     * {@link FluidState#isEmpty() FluidState::isEmpty} should typically be checked at least once before this method.
     *
     * @return This as a source block.
     * @throws UnsupportedOperationException If this FluidState is empty.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public FluidState toSource() { return withLevel(0); }

    /**
     * {@link FluidState#isValid() FluidState::isValid} should typically be checked at least once before this method.
     *
     * @param level The fluid level of the FluidState to be returned.
     * @return The FluidState that has the provided fluid level and that has this FluidState's other properties.
     *         This method should be used instead of <blockquote>"state.withProperty(BlockLiquid.LEVEL, level)"</blockquote> wherever possible.
     *
     * @throws UnsupportedOperationException If this FluidState is empty.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public FluidState withLevel(final int level) {
        // create the FluidState lookup array for this block's BlockStateContainer if it does not yet exist
        @Nonnull final ILevelFluidStateLookup lookup = (ILevelFluidStateLookup)getBlock().getBlockState();
        if(lookup.getFluidStateLookup() == null) {
            @Nonnull final IBlockState[] states = getBlock().getBlockState().getValidStates().stream()
                    .filter(stateIn -> FluidState.of(stateIn).getLevel() == 0)
                    .toArray(IBlockState[]::new);

            // create and fill the FluidState lookup array
            lookup.setFluidStateLookup(new FluidState[states.length][BlockLiquid.LEVEL.getAllowedValues().size()]);
            for(final int[] stateId = {0}; stateId[0] < states.length; stateId[0]++) BlockLiquid.LEVEL.getAllowedValues().forEach(lvl ->
                (lookup.getFluidStateLookup()[stateId[0]][lvl] = FluidState.of(states[stateId[0]].withProperty(BlockLiquid.LEVEL, lvl))).propsKey = stateId[0]);
        }

        // get FluidState from lookup
        return lookup.getFluidStateLookup()[propsKey][level];
    }

    /**
     * {@link FluidState#isValid() FluidState::isValid} should typically be checked at least once before this method.
     *
     * @param toAdd The fluid level to add to this one.
     * @return The FluidState that has a level equal to (level + toAdd) (capped at quantaPerBlock - 1), and that has
     * this FluidState's other properties.
     *
     * @throws UnsupportedOperationException If this FluidState is empty.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public FluidState addLevel(final int toAdd) {
        if(getBlock() instanceof BlockFluidFinite) return withLevel(Math.max(getLevel() - toAdd, 0));
        else return withLevel(Math.min(getLevel() + toAdd, Math.max(getQuantaPerBlock() - 1, 0)));
    }

    /**
     * @return This FluidState as a flowing liquid block if it's a static liquid block, otherwise returns itself.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public FluidState toFlowing() {
        return getBlock() instanceof BlockStaticLiquid ? of(BlockLiquid.getFlowingBlock(getMaterial())).withLevel(getLevel()) : this;
    }

    /**
     * @return This FluidState as a static liquid block if it's a flowing liquid block, otherwise returns itself.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public FluidState toStatic() {
        return getBlock() instanceof BlockDynamicLiquid ? of(BlockLiquid.getStaticBlock(getMaterial())).withLevel(getLevel()) : this;
    }

    public float getQuantaFraction() {
        return getBlock() instanceof PluginBlockFluidBase.Accessor ? ((PluginBlockFluidBase.Accessor)getBlock()).getQuantaFraction_Public() : 8f/9;
    }

    public int getQuantaPerBlock() {
        return getBlock() instanceof PluginBlockFluidBase.Accessor ? ((PluginBlockFluidBase.Accessor)getBlock()).getQuantaPerBlock_Public() : 8;
    }

    public float getQuantaPerBlockFloat() {
        return getBlock() instanceof PluginBlockFluidBase.Accessor ? ((PluginBlockFluidBase.Accessor)getBlock()).getQuantaPerBlockFloat_Public() : 8f;
    }

    public int getQuantaValue() {
        return getBlock() instanceof BlockFluidFinite ? getLevel() + 1 : getQuantaPerBlock() - getLevel();
    }

    public final int getQuantaValue(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
        @Nonnull final IBlockAccess access = world instanceof World ? new FluidCache(world, pos, 0, 0) : world;
        if(access.isAirBlock(pos)) return 0;

        @Nonnull final FluidState fluidState = FluidloggedUtils.getFluidState(access, pos);
        return FluidloggedUtils.isCompatibleFluid(this, fluidState) ? fluidState.getQuantaValue() : -1;
    }

    public final int getQuantaValueAbove(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, final int aboveThis) {
        final int quantaRemaining = getQuantaValue(world, pos);
        return quantaRemaining <= aboveThis ? -1 : quantaRemaining;
    }

    public final int getQuantaValueBelow(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, final int belowThis) {
        final int quantaRemaining = getQuantaValue(world, pos);
        return quantaRemaining >= belowThis ? -1 : quantaRemaining;
    }

    // ===================================================
    // UTILITY ACCESSOR METHODS FOR FLUID BLOCK PROPERTIES
    // ===================================================

    public int getFlowCost(@Nonnull final World world) {
        return getBlock() instanceof IFlowCostFluid ? ((IFlowCostFluid)getBlock()).getFlowCost(this, world) : 1;
    }

    public int getDensity() {
        return getBlock() instanceof BlockFluidBase ? ((BlockFluidBase)getBlock()).getDensity() : getFluid().getDensity();
    }

    public int getTemperature() {
        return getBlock() instanceof BlockFluidBase ? ((BlockFluidBase)getBlock()).getTemperature() : getFluid().getTemperature();
    }

    public int getDensityDir() {
        return getBlock() instanceof PluginBlockFluidBase.Accessor ? ((PluginBlockFluidBase.Accessor)getBlock()).getDensityDir_Public() : -1;
    }

    @Nonnull
    public Map<Block, Boolean> getDisplacements() {
        return getBlock() instanceof PluginBlockFluidBase.Accessor ? ((PluginBlockFluidBase.Accessor)getBlock()).getDisplacements_Public() : Collections.emptyMap();
    }

    protected float fluidHeight = Float.MAX_VALUE;
    public float getHeight() {
        if(fluidHeight != Float.MAX_VALUE) return fluidHeight;
        else if(getBlock() instanceof BlockLiquid) return fluidHeight = 1 - BlockLiquid.getLiquidHeightPercent(getLevel() >= 8 ? 1 : getLevel());
        else if(isValid()) return fluidHeight = getQuantaValue() * getQuantaFraction() / getQuantaPerBlockFloat();
        else if(isEmpty()) throw new UnsupportedOperationException("Cannot get fluid height from empty FluidState!");
        else return fluidHeight = getQuantaFraction(); // fallback for badly coded fluid blocks (#255)
    }

    public float getActualHeight(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
        @Nonnull final IBlockAccess access = world instanceof World ? new FluidCache(world, pos, 1, 1) : world;
        @Nonnull final EnumFacing side = getUpDensityFace();

        if(FluidloggedUtils.canFluidFlow(access, pos, access.getBlockState(pos), side)) {
            @Nonnull final BlockPos offset = pos.offset(side);
            @Nonnull final IBlockState above = access.getBlockState(offset);
            return FluidloggedUtils.canFluidFlow(access, offset, above, side.getOpposite())
                    && FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(access, offset, above), this) ? 1 : getHeight();
        }

        return getHeight();
    }

    @Nonnull
    public AxisAlignedBB getFluidBox(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos) {
        final double fluidHeight = getActualHeight(access, pos);
        return getFluid().isLighterThanAir()
                ? new AxisAlignedBB(pos.getX(), pos.getY() + 1 - fluidHeight, pos.getZ(), pos.getX() + 1, pos.getY() + 1, pos.getZ() + 1)
                : new AxisAlignedBB(pos.getX(), pos.getY(), pos.getZ(), pos.getX() + 1, pos.getY() + fluidHeight, pos.getZ() + 1);
    }

    /**
     * {@link FluidState#isValid() FluidState::isValid} should typically be checked at least once before this method.
     *
     * @return A new {@link FluidStack} containing this FluidState's fluid.
     * The returned {@link FluidStack} will have a size of 0 if this FluidState is not a source block.
     * @throws UnsupportedOperationException If this FluidState is not valid.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    public FluidStack createFluidStack() {
        if(getBlock() instanceof BlockFluidFinite) { // finite fluid blocks handle FluidStacks differently
            return new FluidStack(getFluid(), MathHelper.floor(getQuantaValue() / getQuantaPerBlockFloat() * Fluid.BUCKET_VOLUME));
        }

        return new FluidStack(getFluid(), isSource() ? Fluid.BUCKET_VOLUME : 0);
    }

    @Nonnull
    public EnumFacing getDownDensityFace() { return getDensityDir() < 0 ? EnumFacing.DOWN : EnumFacing.UP; }

    @Nonnull
    public EnumFacing getUpDensityFace() { return getDensityDir() < 0 ? EnumFacing.UP : EnumFacing.DOWN; }

    // ===============================================================
    // METHODS FROM PAIR, PLEASE USE getFluid & getState WHEN POSSIBLE
    // ===============================================================

    @Override
    public final Fluid getLeft() { return getFluid(); }

    @Override
    public final IBlockState getRight() { return getState(); }

    @Override
    public final IBlockState getValue() { return getState(); }

    @Override
    public final IBlockState setValue(@Nullable final IBlockState value) { throw new UnsupportedOperationException(); }

    private static final class EmptyFluidState extends FluidState
    {
        EmptyFluidState() {
            super(null, null);
            fluidHeight = 0;
        }

        @Nonnull
        @Override
        public IBlockState getState() { return BlockStateContainer.AIR_BLOCK_STATE; }

        @Override
        public int serialize() { return -1; }

        @Override
        public boolean isEmpty() { return true; }

        @Override
        public int getLevel() { throw new UnsupportedOperationException("Cannot get level from empty FluidState!"); }

        @Override
        public int getDensityDir() { throw new UnsupportedOperationException("Cannot get densityDir from empty FluidState!"); }

        @Override
        public float getQuantaFraction() { throw new UnsupportedOperationException("Cannot get quantaFraction from empty FluidState!"); }

        @Override
        public int getQuantaPerBlock() { throw new UnsupportedOperationException("Cannot get quantaPerBlock from empty FluidState!"); }

        @Override
        public float getQuantaPerBlockFloat() { throw new UnsupportedOperationException("Cannot get quantaPerBlockFloat from empty FluidState!"); }

        @Nonnull
        @Override
        public FluidState withLevel(final int level) { throw new UnsupportedOperationException("Cannot apply level to empty FluidState!"); }

        @Nonnull
        @Override
        public AxisAlignedBB getFluidBox(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos) {
            throw new UnsupportedOperationException("Cannot get box from empty FluidState!");
        }
    }
}
