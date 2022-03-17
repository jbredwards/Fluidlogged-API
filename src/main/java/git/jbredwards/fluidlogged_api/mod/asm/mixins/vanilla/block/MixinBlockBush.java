package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import net.minecraft.block.BlockBush;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * breaking this block type no longer voids the possible FluidState here
 * @author jbred
 *
 */
@Mixin(BlockBush.class)
public abstract class MixinBlockBush
{
    @Redirect(method = "checkAndDropBlock", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"))
    private boolean getFluidOrAir(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState air, int blockFlags) { return world.setBlockToAir(pos); }
}
