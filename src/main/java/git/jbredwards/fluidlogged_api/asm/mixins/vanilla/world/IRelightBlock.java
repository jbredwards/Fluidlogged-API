package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.world;

import net.minecraft.util.math.BlockPos;

import javax.annotation.Nonnull;

/**
 * used by {@link git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils#relightFluidBlock}
 * @author jbred
 *
 */
public interface IRelightBlock
{
    void relightBlock(@Nonnull BlockPos pos);
    int getHeight(@Nonnull BlockPos pos);
}
