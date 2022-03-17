package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.BlockSkull;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * makes skulls fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockSkull.class)
public abstract class MixinBlockSkull implements IFluidloggable
{
    @Redirect(method = "checkWitherSpawn", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z", ordinal = 1))
    private boolean getFluidOrAir(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState air, int blockFlags) {
        final FluidState fluidState = FluidState.get(world, pos);
        return world.setBlockState(pos, fluidState.isEmpty() ? air : fluidState.getState(), blockFlags);
    }
}
