package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.BlockWall;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * makes walls fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockWall.class)
public abstract class MixinBlockWall implements IFluidloggable
{
    @Redirect(method = "getActualState", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;isAirBlock(Lnet/minecraft/util/math/BlockPos;)Z"))
    private boolean isAirBlock(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return world.isAirBlock(pos) || FluidloggedUtils.getFluidFromState(world.getBlockState(pos)) != null;
    }
}
