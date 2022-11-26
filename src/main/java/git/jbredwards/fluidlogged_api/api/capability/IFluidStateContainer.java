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
    void forEach(@Nonnull BiConsumer<Character, FluidState> action);
    boolean hasFluidState(char serializedPos);

    char serializePos(@Nonnull BlockPos pos);
    @Nonnull BlockPos deserializePos(char serializedPos);

    //internal, just make sure to sync any changes between the server & client!
    void clearFluidStates();
    void setFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fluidState);
    void setFluidState(char serializedPos, @Nonnull FluidState fluidState);
    @Nonnull FluidState getFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fallback);
    @Nonnull FluidState getFluidState(char serializedPos, @Nonnull FluidState fallback);
}
