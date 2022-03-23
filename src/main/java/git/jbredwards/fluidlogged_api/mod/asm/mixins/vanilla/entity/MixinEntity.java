package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.EntityBoat;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeHooks;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * onEntityCollidedWithBlock & isInsideOfMaterial work with FluidStates
 * @author jbred
 *
 */
@Mixin(Entity.class)
public abstract class MixinEntity
{
    @Shadow
    public World world;

    @Shadow
    public double posX, posY, posZ;

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite
    public boolean isInsideOfMaterial(@Nonnull Material materialIn) {
        if(getRidingEntity() instanceof EntityBoat) return false;

        final double yToTest = posY + getEyeHeight();
        final BlockPos pos = new BlockPos(posX, yToTest, posZ);
        final IBlockState state = world.getBlockState(pos);

        @Nullable Boolean result = state.getBlock().isEntityInsideMaterial(world, pos, state, (Entity)(Object)this, yToTest, materialIn, true);
        if(result != null) return result;

        if(FluidloggedUtils.getFluidFromState(state) == null) {
            final FluidState fluidState = FluidState.get(world, pos);
            if(!fluidState.isEmpty()) {
                result = fluidState.getBlock().isEntityInsideMaterial(world, pos, fluidState.getState(), (Entity)(Object)this, yToTest, materialIn, true);

                if(result != null) return result;
                else if(fluidState.getMaterial() == materialIn)
                    return ForgeHooks.isInsideOfMaterial(materialIn, (Entity)(Object)this, pos);
            }
        }

        return state.getMaterial() == materialIn && ForgeHooks.isInsideOfMaterial(materialIn, (Entity)(Object)this, pos);
    }

    @Redirect(method = "doBlockCollisions", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/Block;onEntityCollidedWithBlock(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;)V", remap = false))
    private void onEntityCollidedWithBlock(@Nonnull Block instance, @Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Entity entityIn) {
        instance.onEntityCollidedWithBlock(worldIn, pos, state, entityIn);
        if(FluidloggedUtils.getFluidFromBlock(instance) != null) return;

        final FluidState fluidState = FluidState.get(worldIn, pos);
        if(!fluidState.isEmpty()) fluidState.getBlock().onEntityCollidedWithBlock(worldIn, pos, fluidState.getState(), entityIn);
    }

    @Shadow
    public abstract float getEyeHeight();

    @Nullable
    @Shadow
    public abstract Entity getRidingEntity();
}
