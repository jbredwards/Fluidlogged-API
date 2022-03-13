package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * block fog respects FluidStates
 * @author jbred
 *
 */
@Mixin(ActiveRenderInfo.class)
public abstract class ActiveRenderInfoMixin
{
    @Nonnull
    @Redirect(method = "getBlockStateAtEntityViewpoint", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private static IBlockState getFluidOrReal(@Nonnull World world, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(world, pos); }
}
