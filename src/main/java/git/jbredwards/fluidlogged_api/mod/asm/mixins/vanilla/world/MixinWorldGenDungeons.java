package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.world;

import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.gen.feature.WorldGenDungeons;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * spawner dungeons now void FluidStates when they generate
 * @author jbred
 *
 */
@Mixin(WorldGenDungeons.class)
public abstract class MixinWorldGenDungeons
{
    @Redirect(method = "generate", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockToAir(Lnet/minecraft/util/math/BlockPos;)Z"))
    private boolean setBlockToAir(@Nonnull World world, @Nonnull BlockPos pos) { return world.setBlockState(pos, Blocks.AIR.getDefaultState()); }

    @Redirect(method = "generate", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"))
    private boolean setBlockState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, int blockFlags) { return world.setBlockState(pos, state, blockFlags | 32); }
}
