package git.jbredwards.fluidlogged_api.common.util;

import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.fluids.Fluid;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * holds a fluid & a block state
 * @author jbred
 *
 */
public class FluidState extends Pair<Fluid, IBlockState>
{
    //always used instead of a null value
    public static final FluidState EMPTY = new FluidState(null, null);

    protected final Fluid fluid;
    protected final IBlockState state;

    protected FluidState(Fluid fluidIn, IBlockState stateIn) {
        fluid = fluidIn;
        state = stateIn;
    }

    //build a new fluid state or return empty instance, if fluid isn't valid for fluidlogging
    @Nonnull
    public static FluidState of(@Nullable Fluid fluidIn) {
        final @Nullable Block block = fluidIn == null ? null : fluidIn.getBlock();
        return block == null ? EMPTY : new FluidState(fluidIn, block instanceof BlockLiquid
                //ensure flowing blocks are used for vanilla fluids
                ? BlockLiquid.getFlowingBlock(block.getDefaultState().getMaterial()).getDefaultState()
                : block.getDefaultState());
    }

    //gets the stored state present in the world at the block pos
    @Nonnull
    public static FluidState get(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final @Nullable Chunk chunk = FluidloggedUtils.getChunk(world, pos);
        return chunk == null ? EMPTY : getFromProvider(chunk, pos);
    }

    //gets the stored state present in the capability provider (usually chunk) at the block pos
    @Nonnull
    public static FluidState getFromProvider(@Nullable ICapabilityProvider p, @Nonnull BlockPos pos) {
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(p);
        return cap == null ? EMPTY : cap.getFluidState(pos);
    }

    public boolean isEmpty() { return this == EMPTY; }

    public Fluid getFluid() { return fluid; }

    public IBlockState getState() { return state; }

    public Block getBlock() { return state.getBlock(); }

    //implemented from Pair, please use methods above instead if possible

    @Override
    @Nonnull
    public final Fluid getLeft() { return getFluid(); }

    @Override
    @Nonnull
    public final IBlockState getRight() { return getState(); }

    @Override
    @Nonnull
    public final IBlockState getValue() { return super.getValue(); }

    @Override
    @Nonnull
    public final IBlockState setValue(@Nullable IBlockState value) { throw new UnsupportedOperationException(); }
}
