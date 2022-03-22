package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.client;

import net.minecraft.client.particle.Particle;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * particles aren't dark in certain fluidlogged blocks
 * @author jbred
 *
 */
@Mixin(Particle.class)
public abstract class MixinParticle
{
    @Redirect(method = "getBrightnessForRender", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getCombinedLight(Lnet/minecraft/util/math/BlockPos;I)I"))
    private int getBrightnessForRender(@Nonnull World world, @Nonnull BlockPos pos, int zero) {
        final int sky = getLightFromNeighborsFor(world, EnumSkyBlock.SKY, pos);
        final int block = getLightFromNeighborsFor(world, EnumSkyBlock.BLOCK, pos);
        return sky << 20 | Math.max(block, 0) << 4;
    }

    private int getLightFromNeighborsFor(@Nonnull World world, @Nonnull EnumSkyBlock type, @Nonnull BlockPos pos) {
        if(!world.provider.hasSkyLight() && type == EnumSkyBlock.SKY) return 0;

        if(pos.getY() < 0) pos = new BlockPos(pos.getX(), 0, pos.getZ());
        if(!world.isValid(pos)) return type.defaultLightValue;

        return Math.max(world.getLightFor(type, pos),
                Math.max(world.getLightFor(type, pos.up()),
                        Math.max(world.getLightFor(type, pos.east()),
                                Math.max(world.getLightFor(type, pos.west()),
                                        Math.max(world.getLightFor(type, pos.south()),
                                                world.getLightFor(type, pos.north()))))));
    }
}
