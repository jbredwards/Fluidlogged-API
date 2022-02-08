package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import net.minecraft.block.Block;
import net.minecraft.block.BlockStairs;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

import static git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockFluidBase.class)
public abstract class BlockFluidBaseMixin extends Block
{
    @Shadow(remap = false)
    protected int quantaPerBlock, densityDir;

    @Final
    @Shadow(remap = false)
    protected Fluid definedFluid;

    public BlockFluidBaseMixin(@Nonnull Material materialIn) { super(materialIn); }

    /*@Nonnull
    @Overwrite
    public Vec3d getFlowVector(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {

    }*/

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite(remap = false)
    final boolean hasVerticalFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final EnumFacing facing = (densityDir < 0) ? EnumFacing.UP : EnumFacing.DOWN;
        if(!canFluidFlow(world, pos, world.getBlockState(pos), facing)) return false;

        final BlockPos offset = pos.down(densityDir);
        final IBlockState state = world.getBlockState(offset);

        return canFluidFlow(world, offset, state, facing.getOpposite())
                && isCompatibleFluid(getFluidState(world, offset, state).getFluid(), getFluid());
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite(remap = false)
    protected boolean causesDownwardCurrent(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState state = worldIn.getBlockState(pos);
        if(canFluidFlow(worldIn, pos, state, side) && isCompatibleFluid(getFluidState(worldIn, pos, state).getFluid(), getFluid())) return false;
        else if(side == (densityDir < 0 ? EnumFacing.UP : EnumFacing.DOWN)) return true;
        else if(state.getMaterial() == Material.ICE) return false;
        else {
            final Block block = state.getBlock();
            boolean flag = isExceptBlockForAttachWithPiston(block) || block instanceof BlockStairs;
            return !flag && state.getBlockFaceShape(worldIn, pos, side) == BlockFaceShape.SOLID;
        }
    }

    /**
     * @reason expose definedFluid to significantly boost performance
     * @author jbred
     */
    @Nonnull
    @Overwrite(remap = false)
    public Fluid getFluid() { return definedFluid; }

    @Nonnull
    @Redirect(method = "getDensity(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"), remap = false)
    private static IBlockState getDensity(@Nonnull IBlockAccess world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Nonnull
    @Redirect(method = "getTemperature(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"), remap = false)
    private static IBlockState getTemperature(@Nonnull IBlockAccess world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Nonnull
    @Redirect(method = "getFlowDirection", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"), remap = false)
    private static IBlockState getFlowDirection(@Nonnull IBlockAccess world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Nonnull
    @Redirect(method = "getFogColor", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFogColor(@Nonnull World world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Nonnull
    @Redirect(method = "getStateAtViewpoint", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFogColor(@Nonnull IBlockAccess world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Shadow(remap = false)
    protected abstract int getFlowDecay(@Nonnull IBlockAccess world, @Nonnull BlockPos pos);
}
