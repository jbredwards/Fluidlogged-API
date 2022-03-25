package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.mixins.utils.IMixinBlock;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
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
@Mixin(Block.class)
public abstract class MixinBlock implements IMixinBlock
{
    /**
     * @reason fixes fluidlogged block lighting
     * @author jbred
     */
    @Overwrite(remap = false)
    public int getLightValue(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(FluidloggedUtils.getFluidFromState(state) != null) return state.getLightValue();
        //return the greater of the two possible light values here
        final FluidState fluidState = FluidState.get(world, pos);
        return Math.max(fluidState.isEmpty() ? 0 : fluidState.getState().getLightValue(world, pos), state.getLightValue());
    }

    /**
     * @reason fixes fluidlogged block lighting
     * @author jbred
     */
    @Overwrite(remap = false)
    public int getLightOpacity(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(FluidloggedUtils.getFluidFromState(state) != null) return state.getLightOpacity();
        //return the greater of the two possible opacity values here
        final FluidState fluidState = FluidState.get(world, pos);
        return Math.max(fluidState.isEmpty() ? 0 : fluidState.getState().getLightOpacity(world, pos), state.getLightOpacity());
    }

    /**
     * @reason explosions take fluidlogged fluids into account
     * @author jbred
     */
    @Overwrite(remap = false)
    public float getExplosionResistance(@Nonnull World world, @Nonnull BlockPos pos, @Nullable Entity exploder, @Nonnull Explosion explosion) {
        if(FluidloggedUtils.getFluidFromState(getDefaultState()) != null) return getExplosionResistance(exploder);
        //return the greater of the two possible resistance values here
        final FluidState fluidState = FluidState.get(world, pos);
        return Math.max(fluidState.isEmpty() ? 0 : fluidState.getBlock().getExplosionResistance(world, pos, exploder, explosion), getExplosionResistance(exploder));
    }

    /**
     * @reason increases method performance & flexibility
     * @author jbred
     */
    @Overwrite
    public boolean isReplaceable(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) { return getDefaultState().getMaterial().isReplaceable(); }

    @Redirect(method = "removedByPlayer", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"))
    private boolean getFluidOrAir(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState air, int blockFlags) {
        final FluidState fluidState = FluidState.get(world, pos);
        return world.setBlockState(pos, fluidState.isEmpty() ? air : fluidState.getState(), blockFlags);
    }

    @Nonnull
    @Redirect(method = "canSustainPlant", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"), remap = false)
    private IBlockState getFluidOrReal(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(world, pos); }

    @Shadow
    public abstract IBlockState getDefaultState();

    @Shadow
    public abstract float getExplosionResistance(@Nullable Entity exploder);

    //IMixinBlock

    @Nullable private Boolean canFluidFlow = null;

    @Nullable
    @Override
    public Boolean getCanFluidFlow() { return canFluidFlow; }

    @Override
    public void setCanFluidFlow(@Nullable Boolean canFluidFlowIn) { canFluidFlow = canFluidFlowIn; }
}
