package git.jbredwards.fluidlogged_api.api.asm.impl;

import com.google.common.collect.Lists;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.world.chunk.ChunkPrimer;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * {@link ChunkPrimer} extends this at runtime to allow for more optimized FluidState world generation
 * @author jbred
 *
 */
public abstract class IFluidStatePrimer
{
    @Nonnull
    protected final List<FluidState> keys = Lists.newArrayList(FluidState.EMPTY);
    protected char[] fluidData = new char[65536];

    @Nonnull
    public static IFluidStatePrimer of(@Nonnull Object chunkPrimer) { return (IFluidStatePrimer)chunkPrimer; }

    @Nonnull
    public FluidState getFluidState(int x, int y, int z) { return keys.get(fluidData[getIndex(x, y, z)]); }

    public void setFluidState(int x, int y, int z, @Nonnull FluidState fluidState) {
        if(fluidState.isEmpty()) fluidData[getIndex(x, y, z)] = 0;
        else {
            int indexedState = keys.indexOf(fluidState);
            if(indexedState == -1) {
                indexedState = keys.size();
                keys.add(fluidState);
            }

            fluidData[getIndex(x, y, z)] = (char)indexedState;
        }
    }

    protected static int getIndex(int x, int y, int z) { return x << 12 | z << 8 | y; }
}
