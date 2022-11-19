package git.jbredwards.fluidlogged_api.api.util;

import git.jbredwards.fluidlogged_api.api.asm.impl.IChunkProvider;
import git.jbredwards.fluidlogged_api.api.asm.impl.IFluidStateProvider;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.IFluidBlock;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.concurrent.Immutable;

/**
 * holds a fluid & a block state
 * @author jbred
 *
 */
@Immutable
public class FluidState extends Pair<Fluid, IBlockState>
{
    //always used instead of a null value
    @Nonnull public static final FluidState EMPTY = new FluidState(null, Blocks.AIR.getDefaultState());

    protected final Fluid fluid;
    protected final IBlockState state;
    protected int level = -1;

    //using FluidState#of rather than the constructor directly is advised
    public FluidState(@Nullable Fluid fluidIn, @Nonnull IBlockState stateIn) {
        fluid = fluidIn;
        state = stateIn;
    }

    //return a FluidState with the input Fluid, or return empty instance if the fluid isn't valid for fluidlogging
    @Nonnull
    public static FluidState of(@Nullable Fluid fluidIn) {
        if(fluidIn == null || !fluidIn.canBePlacedInWorld()) return EMPTY;
        final FluidState defaultFluidState = IFluidStateProvider.getDefaultFluidState(fluidIn);
        //use the fluid's default state if present
        if(!defaultFluidState.isEmpty()) return defaultFluidState;
        //generate new instance if default not present
        final Block block = fluidIn.getBlock();
        final FluidState fluidState = new FluidState(fluidIn, (block instanceof BlockLiquid)
                //ensure flowing blocks are used for vanilla fluids
                ? BlockLiquid.getStaticBlock(block.getDefaultState().getMaterial()).getDefaultState()
                : block.getDefaultState());

        IFluidStateProvider.setDefaultFluidState(fluidIn, fluidState);
        return fluidState;
    }

    //convenience method that takes in a Block
    @Nonnull
    public static FluidState of(@Nullable Block fluidIn) { return of(FluidloggedUtils.getFluidFromBlock(fluidIn)); }

    //convenience method that takes in an IBlockState
    @Nonnull
    public static FluidState of(@Nullable IBlockState fluidIn) { return of(FluidloggedUtils.getFluidFromState(fluidIn)); }

    //gets the stored state present in the world at the block pos
    @Nonnull
    public static FluidState get(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return getFromProvider(IChunkProvider.getChunk(world, pos), pos);
    }

    //gets the stored state from Minecraft#world instance, mainly used for rendering
    @Nonnull
    @SideOnly(Side.CLIENT)
    public static FluidState get(@Nonnull BlockPos pos) {
        return getFromProvider(IChunkProvider.getClientChunk(pos), pos);
    }

    //gets the stored state present in the capability provider (usually chunk) at the block pos
    @Nonnull
    public static FluidState getFromProvider(@Nullable ICapabilityProvider p, @Nonnull BlockPos pos) {
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(p);
        return cap == null ? EMPTY : cap.getContainer(pos).getFluidState(pos, EMPTY);
    }

    //creates a new FluidState from the serialized one
    @Nonnull
    public static FluidState deserialize(int serialized) { return of(Block.getBlockById(serialized)); }

    //converts this FluidState to an int, which can be used to form a new FluidState at a later time
    public int serialize() { return isEmpty() ? 0 : Block.getIdFromBlock(getBlock()); }

    public boolean isEmpty() { return this == EMPTY; }

    //null if empty, nonnull otherwise
    public Fluid getFluid() { return fluid; }

    @Nonnull
    public IBlockState getState() { return state; }

    //some FluidStates may contain badly coded fluid blocks if gotten from an IBlockState
    //it's advised to check for this before running IFluidBlock logic
    public boolean isValid() { return state.getBlock() instanceof IFluidBlock; }

    @Nonnull
    public IFluidBlock getFluidBlock() {
        if(isValid()) return (IFluidBlock)state.getBlock();
        else throw new IllegalStateException(
                "Invalid FluidState, please report this to the Fluidlogged API bug tracker!"
        );
    }

    @Nonnull
    public Block getBlock() { return state.getBlock(); }

    @Nonnull
    public Material getMaterial() { return state.getMaterial(); }

    public int getLevel() { return level >= 0 ? level : (level = state.getValue(BlockLiquid.LEVEL)); }

    @Override
    public final Fluid getLeft() { return getFluid(); }

    @Override
    public final IBlockState getRight() { return getState(); }

    @Override
    public final IBlockState getValue() { return super.getValue(); }

    @Override
    public final IBlockState setValue(@Nullable IBlockState value) { throw new UnsupportedOperationException(); }
}
