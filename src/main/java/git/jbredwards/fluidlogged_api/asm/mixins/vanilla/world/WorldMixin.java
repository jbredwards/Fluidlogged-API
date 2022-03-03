package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.world;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
@Mixin(World.class)
public abstract class WorldMixin implements IBlockAccess
{
    @Nonnull
    @Redirect(method = {"getLight(Lnet/minecraft/util/math/BlockPos;Z)I", "getLightFromNeighborsFor"}, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFluidOrReal(@Nonnull World self, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(self, pos); }

    /*@Nullable
    @Redirect(method = "isMaterialInBB", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/Block;isAABBInsideMaterial(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;"))
    private static Boolean isMaterialInBB(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb, @Nonnull Material materialIn) {
        final @Nullable Boolean oldResult = block.isAABBInsideMaterial(world, pos, bb, materialIn);
        if(oldResult != null) return oldResult;

        final FluidState fluidState = FluidState.get(world, pos);
        if(fluidState.isEmpty()) return null;

        final @Nullable Boolean newResult = fluidState.getBlock().isAABBInsideMaterial(world, pos, bb, materialIn);
        if(newResult != null) return newResult;

        else return block.getDefaultState().getMaterial() == materialIn || fluidState.getMaterial() == materialIn;
    }*/
}
