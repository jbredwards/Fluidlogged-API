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
    default boolean isFluidValid(IBlockState here, @Nullable Fluid fluid) {
        return true;
    }

    //sets the fluidlogged state when this becomes fluidlogged
    default IBlockState getFluidloggedState(World world, BlockPos pos, IBlockState old) { return old; }

    //sets the non-fluidlogged state when this becomes non-fluidlogged
    default IBlockState getNonFluidloggedState(World world, BlockPos pos, IBlockState old) { return old; }

    //restore optional cache from storedNBT or do other things
    default void readFromStoredNBT(TileEntityFluidlogged te) {}

    //save optional cache to storedNBT or do other things
    default void writeToStoredNBT(TileEntityFluidlogged te) {}

    //fired each tick by the tile entity while it's fluidlogged
    default void fluidloggedTick(TileEntityFluidlogged te) {}

    //return true if the TESR should be ran
    @SideOnly(Side.CLIENT)
    default boolean doFluidloggedTESR(TileEntityFluidlogged te) { return false; }

    //renders using the TESR, note that the special renderer does nothing except execute this code
    @SideOnly(Side.CLIENT)
    default void runFluidloggedTESR(TileEntityFluidlogged te, double x, double y, double z, float partialTicks, int destroyStage, float alpha) {}

    //return false if the fluid should not be rendered while fluidlogged
    @SideOnly(Side.CLIENT)
    default boolean shouldFluidRender(IBlockState stored, @Nullable Fluid fluid, IBlockAccess world, BlockPos pos) { return true; }

    //returns true if the block can flow in the given direction
    default boolean canSideFlow(IBlockState stored, @Nullable Fluid fluid, IBlockAccess world, BlockPos pos, EnumFacing side) {
        return stored.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }
}
