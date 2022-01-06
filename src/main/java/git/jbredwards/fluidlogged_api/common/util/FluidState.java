package git.jbredwards.fluidlogged_api.common.util;

import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.fluids.Fluid;
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
    @Nonnull public static final FluidState EMPTY = new FluidState(null, null);

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
    public static FluidState get(@Nullable IBlockAccess world, @Nonnull BlockPos pos) {
        return getFromProvider(FluidloggedUtils.getChunk(world, pos), pos);
    }

    //gets the stored state present in the client world at the block pos
    @SideOnly(Side.CLIENT)
    @Nonnull
    public static FluidState get(@Nonnull BlockPos pos) { return get(null, pos); }

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

    public Material getMaterial() { return state.getMaterial(); }

    public int getLevel() { return state.getValue(BlockLiquid.LEVEL); }

    //creates a new FluidState from the serialized one
    @Nonnull
    public static FluidState deserialize(int serialized) {
        return of(FluidloggedUtils.getFluidFromBlock(Block.getBlockById(serialized)));
    }

    //converts this FluidState to an int, which can be used to form a new FluidState at a later time
    public int serialize() { return isEmpty() ? 0 : Block.getIdFromBlock(getBlock()); }

    @Nonnull
    @Override
    public String toString() { return isEmpty() ? "EMPTY" : super.toString(); }

    @Nonnull
    @Override
    public String toString(@Nonnull String format) {
        return isEmpty() ? String.format(format, "EMPTY") : super.toString(format);
    }

    @Override
    public final Fluid getLeft() { return getFluid(); }

    @Override
    public final IBlockState getRight() { return getState(); }

    @Override
    public final IBlockState getValue() { return super.getValue(); }

    @Override
    public final IBlockState setValue(@Nullable IBlockState value) { throw new UnsupportedOperationException(); }
}
