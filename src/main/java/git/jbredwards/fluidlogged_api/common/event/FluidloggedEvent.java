package git.jbredwards.fluidlogged_api.common.event;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fml.common.eventhandler.Cancelable;
import net.minecraftforge.fml.common.eventhandler.Event;

import javax.annotation.Nonnull;

/**
 * fired when trying to fluidlog/unfluidlog a block
 * if cancelled, the block will not become fluidlogged
 * result default = default code gets ran; result allow = block was fluidlogged; result deny = block was not fluidlogged
 * @author jbred
 *
 */
@Cancelable
@Event.HasResult
public class FluidloggedEvent extends Event
{
    @Nonnull public final World world;
    @Nonnull public final BlockPos pos;
    @Nonnull public final IBlockState state;
    @Nonnull public FluidState fluidState;
    public boolean checkVaporize;
    public int flags;

    public FluidloggedEvent(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull FluidState fluidState, boolean checkVaporize, int flags) {
        this.world = world;
        this.pos = pos;
        this.state = state;
        this.fluidState = fluidState;
        this.checkVaporize = checkVaporize;
        this.flags = flags;
    }
}
