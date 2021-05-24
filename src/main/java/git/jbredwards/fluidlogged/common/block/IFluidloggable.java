package git.jbredwards.fluidlogged.common.block;

import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * implement this if the block can be fluidlogged (but also can be not fluidlogged, like sea pickles)
 * this interface supports the forge optional @interface as well, so iuf you want your mod to have optional support for this one, you can!
 * @author jbred
 *
 */
public interface IFluidloggable
{
    //return true if the block can be fluidlogged with the input fluid
    //if the fluid is null, always return true
    default boolean isFluidValid(@Nonnull IBlockState here, @Nullable Fluid fluid) {
        return true;
    }

    //sets the fluidlogged state when this becomes fluidlogged
    @Nonnull
    default IBlockState getFluidloggedState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState old) {
        return old;
    }

    //sets the non-fluidlogged state when this becomes non-fluidlogged
    @Nonnull
    default IBlockState getNonFluidloggedState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState old) {
        return old;
    }

    //fired each tick by the tile entity while it's fluidlogged
    default void fluidloggedTick(@Nonnull TileEntityFluidlogged te) {}

    //renders using the te special renderer
    @SideOnly(Side.CLIENT)
    default void fluidloggedRenderTick(@Nonnull TileEntityFluidlogged te, double x, double y, double z, float partialTicks, int destroyStage, float alpha) {}
}
