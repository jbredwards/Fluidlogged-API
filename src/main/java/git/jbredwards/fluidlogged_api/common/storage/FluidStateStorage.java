package git.jbredwards.fluidlogged_api.common.storage;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.util.math.BlockPos;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public class FluidStateStorage
{
    public final int y;

    public FluidStateStorage(int y) { this.y = y; }

    @Nonnull
    public FluidState get(@Nonnull BlockPos pos) {
        return FluidState.EMPTY;
    }

    public void set(@Nonnull BlockPos pos, @Nonnull FluidState fluidState) {

    }
}
