package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.particle.ParticleBubble;
import net.minecraft.client.particle.ParticleDrip;
import net.minecraft.client.particle.ParticleSuspend;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * this particle doesn't instantly disappear while inside water FluidStates
 * @author jbred
 *
 */
@Mixin({ParticleBubble.class, ParticleDrip.class, ParticleSuspend.class})
public abstract class MixinWaterParticles
{
    @Nonnull
    @Redirect(method = "onUpdate", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFluidOrReal(@Nonnull World world, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(world, pos); }
}
