package git.jbredwards.fluidlogged_api.common.block;

import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import javax.annotation.Nonnull;

/**
 * allows you to change the color of your block breaking particles through code
 * @author jbred
 *
 */
public interface IParticleColor
{
    int getParticleColor(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos);
}
