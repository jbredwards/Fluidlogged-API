package git.jbredwards.fluidlogged_api.mod.common.capability.util;

import git.jbredwards.fluidlogged_api.api.util.FluidState;

import javax.annotation.Nonnull;

/**
 * Holds the data for FluidStates within a 16x1x16 area
 * @author jbred
 *
 */
public class FluidStateLayer
{
    @Nonnull
    public FluidState[] data = new FluidState[0];
    public byte tracker = Byte.MIN_VALUE; //start with min byte value, to prevent possible overflow problems
}
