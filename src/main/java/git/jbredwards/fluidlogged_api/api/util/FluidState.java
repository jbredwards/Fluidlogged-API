package git.jbredwards.fluidlogged_api.api.util;

import git.jbredwards.fluidlogged_api.mod.Main;
import git.jbredwards.fluidlogged_api.mod.asm.mixins.utils.IMixinFluid;
import git.jbredwards.fluidlogged_api.mod.common.capability.IFluidStateCapability;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.IFluidBlock;
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

    //using FluidState#of rather than the constructor directly is advised
    public FluidState(Fluid fluidIn, IBlockState stateIn) {
        fluid = fluidIn;
        state = stateIn;
    }

    //return a FluidState with the input Fluid, or return empty instance if the fluid isn't valid for fluidlogging
    @Nonnull
    public static FluidState of(@Nullable Fluid fluidIn) {
        if(fluidIn == null || !fluidIn.canBePlacedInWorld()) return EMPTY;
        final IMixinFluid impl = (IMixinFluid)fluidIn;

        //use the fluid's default state if present
        if(!impl.isEmpty()) return impl.getDefaultFluidState();

        //generate new instance if default not present
        final Block block = fluidIn.getBlock();
        return impl.setDefaultFluidState(
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
        return getFromProvider(Main.proxy.getChunk(world, pos), pos);
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

    //creates a new FluidState from the serialized one
    @Nonnull
    public static FluidState deserialize(int serialized) { return of(Block.getBlockById(serialized)); }

    //converts this FluidState to an int, which can be used to form a new FluidState at a later time
    public int serialize() { return isEmpty() ? 0 : Block.getIdFromBlock(getBlock()); }

    public boolean isEmpty() { return this == EMPTY; }

    public Fluid getFluid() { return fluid; }

    public IBlockState getState() { return state; }

    @SuppressWarnings("unchecked")
    public <T extends Block & IFluidBlock> T getBlock() { return (T)state.getBlock(); }

    public Material getMaterial() { return state.getMaterial(); }

    public int getLevel() { return state.getValue(BlockLiquid.LEVEL); }

    @Override
    public final Fluid getLeft() { return getFluid(); }

    @Override
    public final IBlockState getRight() { return getState(); }

    @Override
    public final IBlockState getValue() { return super.getValue(); }

    @Override
    public final IBlockState setValue(@Nullable IBlockState value) { throw new UnsupportedOperationException(); }
}
