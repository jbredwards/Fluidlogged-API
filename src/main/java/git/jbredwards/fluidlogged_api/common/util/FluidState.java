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
    //always use instead of a null value
    public static final FluidState EMPTY = new FluidState(null, null);

    public final Fluid fluid;
    public final IBlockState state;

    protected FluidState(Fluid fluidIn, IBlockState stateIn) {
        fluid = fluidIn;
        state = stateIn;
    }

    //build a new fluid state, return empty instance if fluid isn't valid for fluidlogging
    @Nonnull
    public static FluidState of(@Nullable Fluid fluidIn) {
        final @Nullable Block block = fluidIn == null ? null : fluidIn.getBlock();
        return block == null ? EMPTY : new FluidState(fluidIn, block instanceof BlockLiquid
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

    @Override
    @Nonnull
    public Fluid getLeft() { return fluid; }

    @Override
    @Nonnull
    public IBlockState getRight() { return state; }

    @Override
    @Nonnull
    public IBlockState setValue(IBlockState value) { throw new UnsupportedOperationException(); }
}
