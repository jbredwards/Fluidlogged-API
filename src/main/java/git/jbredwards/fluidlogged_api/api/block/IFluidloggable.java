package git.jbredwards.fluidlogged_api.api.block;

import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.eventhandler.Event.Result;

import javax.annotation.Nonnull;

/**
 * implement this if your block can be fluidlogged (but also can be not fluidlogged, like sea pickles)
 * quick reminder that all interfaces support the forge net.minecraftforge.fml.common.Optional @interfaces!
 * (using that allows your blocks to support fluidlogging while making this mod only an optional dependency!)
 * @author jbred
 *
 */
public interface IFluidloggable extends IFluidloggableBase
{
    //return true if this is a fluidloggable block
    default boolean isFluidloggable(@Nonnull IBlockState state) { return true; }

    //return true if this is fluidloggable with the input fluid
    default boolean isFluidValid(@Nonnull IBlockState state, @Nonnull Fluid fluid) { return isFluidloggable(state); }

    //called when the fluid is placed into this
    //DEFAULT = run & return result of the default fill action
    //DENY    = think the fill never happened
    //ALLOW   = think the fill happened
    @Nonnull
    default Result onFluidFill(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Fluid fluid) { return Result.DEFAULT; }

    //called when the fluid is drained from this
    //DEFAULT = run & return result of the default drain action
    //DENY    = think the drain never happened
    //ALLOW   = think the drain happened
    @Nonnull
    default Result onFluidDrain(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Fluid fluid) { return Result.DEFAULT; }
}
