package git.jbredwards.fluidlogged_api.api;

import git.jbredwards.fluidlogged_api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fml.common.Loader;

/**
 * allows for easier optional fluidlogged api mod integration
 * @author jbred
 *
 */
public enum FluidloggedHelper
{
    ;

    //true if the fluidlogged api mod is installed
    public static boolean isInstalled = Loader.isModLoaded("fluidlogged_api");

    //use instead of IBlockAccess::getBlockState whenever possible
    public static IBlockState getBlockState(IBlockAccess world, BlockPos pos) {
        return isInstalled ? FluidloggedUtils.getStoredOrReal(world, pos) : world.getBlockState(pos);
    }

    //use instead of World::setBlockState whenever possible
    public static boolean setBlockState(World world, BlockPos pos, IBlockState state, int flags) {
        return isInstalled ? FluidloggedUtils.setStoredOrReal(world, pos, null, state, flags) : world.setBlockState(pos, state, flags);
    }
}
