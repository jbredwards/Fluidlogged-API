package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.BlockReed;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * sugar cane blocks now recognise water FluidStates
 * @author jbred
 *
 */
@Mixin(BlockReed.class)
public abstract class BlockReedMixin
{
    @Nonnull
    @Redirect(method = "canPlaceBlockAt", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", ordinal = 1))
    private IBlockState getFluidOrReal(@Nonnull World world, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(world, pos); }
}
