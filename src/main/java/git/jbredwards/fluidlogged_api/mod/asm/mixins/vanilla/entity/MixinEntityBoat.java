package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.item.EntityBoat;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * fixes some water FluidState interactions
 * @author jbred
 *
 */
@Mixin(EntityBoat.class)
public abstract class MixinEntityBoat
{
    @Nonnull
    @Redirect(method = {"getWaterLevelAbove", "checkInWater", "getUnderwaterStatus", "updateFallState"},
            at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFluidOrReal(@Nonnull World world, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(world, pos); }
}
