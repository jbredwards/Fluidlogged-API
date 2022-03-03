package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.world;

import git.jbredwards.fluidlogged_api.common.storage.IChunkProvider;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.ChunkCache;
import net.minecraft.world.chunk.Chunk;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
@Mixin(ChunkCache.class)
public abstract class ChunkCacheMixin implements IChunkProvider
{
    @Shadow
    protected int chunkX, chunkZ;

    @Shadow
    protected Chunk[][] chunkArray;

    @Nonnull
    @Redirect(method = "getLightForExt", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/ChunkCache;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFluidOrReal(@Nonnull ChunkCache self, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(self, pos); }

    @Nullable
    @Override
    public Chunk getChunkFromBlockCoords(@Nonnull BlockPos pos) {
        final int x = (pos.getX() >> 4) - chunkX;
        final int z = (pos.getZ() >> 4) - chunkZ;
        return withinBounds(x, z) ? chunkArray[x][z]: null;
    }

    @Shadow(remap = false)
    protected abstract boolean withinBounds(int x, int z);
}
