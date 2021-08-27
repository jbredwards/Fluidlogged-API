package git.jbredwards.fluidlogged_api.common.block;

import mcp.MethodsReturnNonnullByDefault;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nullable;
import javax.annotation.ParametersAreNonnullByDefault;

/**
 * implement this if the block can be fluidlogged (but also can be not fluidlogged, like sea pickles)
 * this interface supports the forge net.minecraftforge.fml.common.Optional @interface as well, so if you want your mod to have optional support for this one, you can!
 * @author jbred
 *
 */
@MethodsReturnNonnullByDefault
@ParametersAreNonnullByDefault
public interface IFluidloggable
{
    //return true if the block can be fluidlogged with the input fluid
    //if the fluid is null, always return true
    default boolean isFluidValid(IBlockState here, @Nullable Fluid fluid) { return true; }

    //sets the fluidlogged state when this becomes fluidlogged
    default IBlockState getFluidloggedState(World world, BlockPos pos, IBlockState old) { return old; }

    //sets the non-fluidlogged state when this becomes non-fluidlogged
    default IBlockState getNonFluidloggedState(World world, BlockPos pos, IBlockState old) { return old; }

    //returns true if the block can flow in the given direction
    default boolean canSideFlow(IBlockState stored, @Nullable Fluid fluid, IBlockAccess world, BlockPos pos, EnumFacing side) {
        return stored.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    //return true if the fluid should be rendered while fluidlogged
    @SideOnly(Side.CLIENT)
    default boolean shouldFluidRender(IBlockState stored, @Nullable Fluid fluid, IBlockAccess world, BlockPos pos) { return true; }

    //tile entity integration has been rewritten and this method is no longer used, please remove it
    @Deprecated
    default void readFromStoredNBT(TileEntityFluidlogged te) {}

    //tile entity integration has been rewritten and this method is no longer used, please remove it
    @Deprecated
    default void writeToStoredNBT(TileEntityFluidlogged te) {}

    //tile entity integration has been rewritten and this method is no longer used, please remove it
    @Deprecated
    default void fluidloggedTick(TileEntityFluidlogged te) {}

    //tile entity integration has been rewritten and this method is no longer used, please remove it
    @Deprecated
    @SideOnly(Side.CLIENT)
    default boolean doFluidloggedTESR(TileEntityFluidlogged te) { return false; }

    //tile entity integration has been rewritten and this method is no longer used, please remove it
    @Deprecated
    @SideOnly(Side.CLIENT)
    default void runFluidloggedTESR(TileEntityFluidlogged te, double x, double y, double z, float partialTicks, int destroyStage, float alpha) {}
}
