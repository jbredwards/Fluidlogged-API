package git.jbredwards.fluidlogged_api.common.util;

import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * allows modded IBlockAccess instances the option to provide chunks in a way that this mod can access them.
 * this is called by FluidloggedUtils#getChunk
 * @author jbred
 *
 */
public interface IChunkProvider
{
    @Nullable Chunk getChunkFromBlockCoords(@Nonnull BlockPos pos);
}
