package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.world;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(Chunk.class)
public abstract class ChunkMixin implements ICapabilityProvider, IRelightBlock
{
    @Shadow
    private boolean loaded;

    @Final
    @Shadow
    public int x, z;

    @Final
    @Shadow
    private int[] heightMap;

    @Final
    @Shadow
    private World world;

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    public int getBlockLightOpacity(@Nonnull BlockPos pos) { return getFluidLightOpacity(getBlockState(pos), pos); }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    private int getBlockLightOpacity(int xIn, int yIn, int zIn) {
        return loaded ? getFluidLightOpacity(getBlockState(xIn, yIn, zIn), new BlockPos(x << 4 | xIn & 15, yIn, z << 4 | zIn & 15)) : getBlockState(xIn, yIn, zIn).getLightOpacity();
    }

    private int getFluidLightOpacity(@Nonnull IBlockState here, @Nonnull BlockPos pos) {
        final FluidState fluidState = FluidState.getFromProvider(this, pos);
        if(fluidState.isEmpty()) return here.getLightOpacity(world, pos);
        else return Math.max(here.getLightOpacity(world, pos), fluidState.getState().getLightOpacity(world, pos));
    }

    private int getFluidLightValue(@Nonnull IBlockState here, @Nonnull BlockPos pos) {
        final FluidState fluidState = FluidState.getFromProvider(this, pos);
        if(fluidState.isEmpty()) return here.getLightValue(world, pos);
        else return Math.max(here.getLightValue(world, pos), fluidState.getState().getLightValue(world, pos));
    }

    @Redirect(method = "setBlockState", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/state/IBlockState;getLightOpacity(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", remap = false))
    private int getFluidLightOpacity(@Nonnull IBlockState here, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) { return getFluidLightOpacity(here, pos); }

    @Redirect(method = {"enqueueRelightChecks", "checkLight(II)Z"}, at = @At(value = "INVOKE", target = "Lnet/minecraft/block/state/IBlockState;getLightValue(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", remap = false))
    private int getFluidLightValue(@Nonnull IBlockState here, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) { return getFluidLightValue(here, pos); }

    @Shadow
    public abstract IBlockState getBlockState(int x, int y, int z);

    @Shadow
    public abstract IBlockState getBlockState(@Nonnull BlockPos pos);

    @Shadow
    protected abstract void relightBlock(int x, int y, int z);

    @Override
    public void relightBlock(@Nonnull BlockPos pos) { relightBlock(pos.getX() & 15, pos.getY(), pos.getZ() & 15); }

    @Override
    public int getHeight(@Nonnull BlockPos pos) { return heightMap[(pos.getZ() & 15) << 4 | (pos.getX() & 15)]; }
}
