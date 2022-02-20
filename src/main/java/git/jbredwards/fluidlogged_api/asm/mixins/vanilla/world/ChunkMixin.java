package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.world;

import git.jbredwards.fluidlogged_api.asm.mixins.utils.IRelightBlock;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.chunk.Chunk;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(Chunk.class)
public abstract class ChunkMixin implements IRelightBlock
{
    @Shadow
    protected abstract void relightBlock(int x, int y, int z);

    @Shadow
    protected abstract void propagateSkylightOcclusion(int x, int z);

    @Shadow
    public abstract int getLightFor(@Nonnull EnumSkyBlock type, @Nonnull BlockPos pos);

    @Override
    public void relightBlock(@Nonnull BlockPos pos) { relightBlock(pos.getX() & 15, pos.getY(), pos.getZ() & 15); }

    @Override
    public void propagateSkylightOcclusion(@Nonnull BlockPos pos) {
        if(getLightFor(EnumSkyBlock.SKY, pos) > 0 || getLightFor(EnumSkyBlock.BLOCK, pos) > 0)
            propagateSkylightOcclusion(pos.getX() & 15, pos.getZ() & 15);
    }
}