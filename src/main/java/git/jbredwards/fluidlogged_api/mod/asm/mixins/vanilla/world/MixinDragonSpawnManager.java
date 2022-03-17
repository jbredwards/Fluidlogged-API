package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.world;

import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.WorldServer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * summoning the ender dragon voids FluidStates at the pillar locations
 * @author jbred
 *
 */
@Mixin(targets = "net.minecraft.world.end.DragonSpawnManager$3")
public abstract class MixinDragonSpawnManager
{
    @Redirect(method = "process", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/WorldServer;setBlockToAir(Lnet/minecraft/util/math/BlockPos;)Z"))
    private boolean process(@Nonnull WorldServer world, @Nonnull BlockPos pos) { return world.setBlockState(pos, Blocks.AIR.getDefaultState()); }
}
