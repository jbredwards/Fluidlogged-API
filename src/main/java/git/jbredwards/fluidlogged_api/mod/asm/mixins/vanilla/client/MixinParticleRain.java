package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.particle.ParticleRain;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * fix rain particle collision
 * @author jbred
 *
 */
@Mixin(ParticleRain.class)
public abstract class MixinParticleRain
{
    @Nonnull
    @Redirect(method = "onUpdate", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/state/IBlockState;getBoundingBox(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/util/math/AxisAlignedBB;"))
    private AxisAlignedBB fixCollision(@Nonnull IBlockState here, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final FluidState fluidState = FluidState.get(world, pos);
        final AxisAlignedBB aabb = here.getBoundingBox(world, pos);

        if(!fluidState.isEmpty() && fluidState.getBlock() instanceof BlockLiquid)
            return aabb.setMaxY(Math.max(aabb.maxY, 1 - BlockLiquid.getLiquidHeightPercent(fluidState.getLevel())));

        else return aabb;
    }
}
