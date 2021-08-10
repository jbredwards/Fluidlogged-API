package git.jbredwards.fluidlogged_api.common.block;

import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import javax.annotation.ParametersAreNonnullByDefault;
import java.util.Random;

/**
 * called by AbstractFluidloggedBlock::updateTick to allow modded fluid blocks to generate fire, or do other things
 * @author jbred
 *
 */
@ParametersAreNonnullByDefault
public interface IFluidFireSpreader
{
    void tryFireSpread(World world, BlockPos pos, IBlockState state, Random rand, EnumFacing[] fluidSides);
}
