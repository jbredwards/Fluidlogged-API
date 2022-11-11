package git.jbredwards.fluidlogged_api.api.capability;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.util.math.BlockPos;

import javax.annotation.Nonnull;
import java.util.function.BiConsumer;

/**
 * Hold FluidStates
 * @author jbred
 *
 */
public interface IFluidStateContainer
{
    //internal methods, only use these if you have to!
    void forEach(@Nonnull BiConsumer<Integer, FluidState> action);
    boolean hasFluidState(int serializedPos);

    int serializePos(@Nonnull BlockPos pos);
    @Nonnull BlockPos deserializePos(int serializedPos);

    //internal, just make sure to sync any changes between the server & client!
    void clearFluidStates();
    void setFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fluidState);
    void setFluidState(int serializedPos, @Nonnull FluidState fluidState);
    @Nonnull FluidState getFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fallback);
    @Nonnull FluidState getFluidState(int serializedPos, @Nonnull FluidState fallback);
}
