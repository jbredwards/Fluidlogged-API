package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.world;

import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings({"OverwriteAuthorRequired", "unused"})
@Mixin(Chunk.class)
public abstract class ChunkMixin
{
    @Final
    @Shadow
    private int[] heightMap, precipitationHeightMap;

    @Nullable
    @Overwrite
    public IBlockState setBlockState(@Nonnull BlockPos pos, @Nonnull IBlockState newState) {
        final int x = pos.getX() & 15;
        final int y = pos.getY();
        final int z = pos.getZ() & 15;

        final int heightIndex = z << 4 | x;
        final int height = heightMap[heightIndex];
        if(y >= precipitationHeightMap[heightIndex] - 1) precipitationHeightMap[heightIndex] = -999;

        final @Nullable IBlockState oldState = getBlockState(pos);
        if(oldState == null) return null;

        final Block oldBlock = oldState.getBlock();
        final Block newBlock = newState.getBlock();

        final int old
    }

    @Shadow
    public abstract IBlockState getBlockState(BlockPos pos);
}
