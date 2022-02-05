package git.jbredwards.fluidlogged_api.common.storage;

import git.jbredwards.fluidlogged_api.asm.mixins.forge.IDefaultFluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.fluids.Fluid;
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

    //return a FluidState with the input Fluid, or return empty instance if the fluid isn't valid for fluidlogging
    @Nonnull
    public static FluidState of(@Nullable Fluid fluidIn) {
        if(!(fluidIn instanceof IDefaultFluidState) || !fluidIn.canBePlacedInWorld())
            return EMPTY;

        //use the fluid's default state if present
        if(!((IDefaultFluidState)fluidIn).isEmpty()) return ((IDefaultFluidState)fluidIn).getDefaultFluidState();

        //generate new instance if default not present
        final Block block = fluidIn.getBlock();
        return ((IDefaultFluidState)fluidIn).setDefaultFluidState(
                new FluidState(fluidIn, (block instanceof BlockLiquid)
                //ensure flowing blocks are used for vanilla fluids
                ? BlockLiquid.getFlowingBlock(block.getDefaultState().getMaterial()).getDefaultState()
                : block.getDefaultState()));
    }

    //convenience method that takes in a Block
    @Nonnull
    public static FluidState of(@Nullable Block fluidIn) { return of(FluidloggedUtils.getFluidFromBlock(fluidIn)); }

    //convenience method that takes in an IBlockState
    @Nonnull
    public static FluidState of(@Nullable IBlockState fluidIn) { return of(FluidloggedUtils.getFluidFromState(fluidIn)); }

    //gets the stored state present in the world at the block pos
    @Nonnull
    public static FluidState get(@Nullable IBlockAccess world, @Nonnull BlockPos pos) {
        return getFromProvider(FluidloggedUtils.getChunk(world, pos), pos);
    }

    //(intended use only by client) gets the stored state from Minecraft#world instance
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
    public static FluidState deserialize(int serialized) { return of(Block.getBlockById(serialized)); }

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
