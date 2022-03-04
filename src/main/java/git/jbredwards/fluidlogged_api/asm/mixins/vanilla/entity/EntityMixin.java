package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.entity;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
@Mixin(Entity.class)
public abstract class EntityMixin
{
    @Redirect(method = "doBlockCollisions", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/Block;onEntityCollidedWithBlock(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;)V"))
    private void onEntityCollidedWithBlock(@Nonnull Block instance, @Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Entity entityIn) {
        instance.onEntityCollidedWithBlock(worldIn, pos, state, entityIn);
        if(FluidloggedUtils.getFluidFromBlock(instance) != null) return;

        final FluidState fluidState = FluidState.get(worldIn, pos);
        if(!fluidState.isEmpty()) fluidState.getBlock().onEntityCollidedWithBlock(worldIn, pos, fluidState.getState(), entityIn);
    }
}
