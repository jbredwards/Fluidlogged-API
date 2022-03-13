package git.jbredwards.fluidlogged_api.api.world;

import git.jbredwards.fluidlogged_api.mod.Main;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * gives server-side IBlockAccess (non-World) instances the option to provide chunks in a way that this mod can access them.
 * this is called by {@link Main.CommonProxy#getChunk(net.minecraft.world.IBlockAccess, BlockPos) Main.proxy.getChunk(IBlockAccess, BlockPos)}
 * @author jbred
 *
 */
public interface IChunkProvider
{
    @Nullable
    Chunk getChunkFromBlockCoords(@Nonnull BlockPos pos);
}
