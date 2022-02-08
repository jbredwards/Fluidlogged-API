package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.world;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(World.class)
public abstract class WorldMixin implements IBlockAccess
{
    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    private int getRawLight(@Nonnull BlockPos pos, @Nonnull EnumSkyBlock lightType) {
        if(lightType == EnumSkyBlock.SKY && canSeeSky(pos)) return 15;

        final IBlockState state = getBlockState(pos);
        final FluidState fluidState = FluidState.get(this, pos);

        int light = (lightType == EnumSkyBlock.SKY) ? 0 : getFluidLightValue(state, fluidState, this, pos);
        int opacity = Math.max(1, getFluidLightOpacity(state, fluidState, this, pos));

        // Forge: fix MC-119932
        if(opacity >= 15) return light;
        else if(light >= 14) return light;

        for(EnumFacing facing : EnumFacing.values()) {
            int neighborLight = getLightFor(lightType, pos.offset(facing)) - opacity;
            if(neighborLight > light) light = neighborLight;
            if(light >= 14) return light;
        }

        return light;
    }

    private static int getFluidLightValue(@Nonnull IBlockState state, @Nonnull FluidState fluidState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(fluidState.isEmpty()) return state.getLightValue(world, pos);
        return Math.max(state.getLightValue(world, pos), fluidState.getState().getLightValue(world, pos));
    }

    private static int getFluidLightOpacity(@Nonnull IBlockState state, @Nonnull FluidState fluidState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(fluidState.isEmpty()) return state.getLightOpacity(world, pos);
        return Math.max(state.getLightOpacity(world, pos), fluidState.getState().getLightOpacity(world, pos));
    }

    @Redirect(method = "checkLightFor", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/Block;getLightOpacity(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", remap = false))
    private static int checkLightFor(@Nonnull Block block, @Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return getFluidLightOpacity(state, FluidState.get(world, pos), world, pos);
    }

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

    @Shadow
    public abstract boolean canSeeSky(@Nonnull BlockPos pos);

    @Shadow
    public abstract int getLightFor(@Nonnull EnumSkyBlock type, @Nonnull BlockPos pos);
}
