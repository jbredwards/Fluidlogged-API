package git.jbredwards.fluidlogged_api.mod.asm.mixins.forge;

import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.Map;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@Mixin(BlockFluidBase.class)
public abstract class MixinBlockFluidBase extends Block
{
    @Shadow(remap = false)
    protected int quantaPerBlock, density, densityDir, tickRate;

    @Shadow(remap = false)
    protected Map<Block, Boolean> displacements;

    @Final
    @Shadow(remap = false)
    protected Fluid definedFluid;

    public MixinBlockFluidBase(@Nonnull Material material) { super(material); }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    public boolean canDisplace(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return true;
        //checks if this & a fluid here are the same
        else if(isCompatibleFluid(getFluid(), getFluidState(world, pos, state).getFluid())) return false;
        //predefined displacements
        else if(displacements.containsKey(state.getBlock())) return displacements.get(state.getBlock());

        final Material material = state.getMaterial();
        if(material.blocksMovement() || material == Material.PORTAL || material == Material.STRUCTURE_VOID) return false;

        final int density = BlockFluidBase.getDensity(world, pos);
        return density == Integer.MAX_VALUE || this.density > density;
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Nonnull
    @Overwrite(remap = false)
    public Vec3d getFlowVector(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState here = world.getBlockState(pos);
        Vec3d vec = Vec3d.ZERO;

        final int decay = getFlowDecay(world, pos);

        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            if(canFluidFlow(world, pos, here, facing)) {
                BlockPos offset = pos.offset(facing);
                if(canFluidFlow(world, offset, world.getBlockState(offset), facing.getOpposite())) {
                    int otherDecay = getFlowDecay(world, offset);

                    if(otherDecay >= quantaPerBlock) {
                        otherDecay = getFlowDecay(world, offset.up(densityDir));

                        if(otherDecay < quantaPerBlock) {
                            int power = otherDecay - (decay - quantaPerBlock);
                            vec = vec.add(facing.getXOffset() * power, 0, facing.getZOffset() * power);
                        }
                    }
                    else {
                        int power = otherDecay - decay;
                        vec = vec.add(facing.getXOffset() * power, 0, facing.getZOffset() * power);
                    }
                }
            }
        }

        return vec.normalize();
    }

    /**
     * @reason fixes fluidlogged interactions
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
     * @reason expose definedFluid to significantly boost performance
     * @author jbred
     */
    @Nonnull
    @Overwrite(remap = false)
    public Fluid getFluid() { return definedFluid; }

    @Nonnull
    @Redirect(method = "getFlowDirection", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"), remap = false)
    private static IBlockState getBlockState(@Nonnull IBlockAccess world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @SideOnly(Side.CLIENT)
    @Nonnull
    @Overwrite(remap = false)
    public Vec3d getFogColor(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Entity entity, @Nonnull Vec3d originalColor, float partialTicks) {
        //remove built-in in place for better check
        if(isWithinFluid(world, pos, ActiveRenderInfo.projectViewFromEntity(entity, partialTicks).y)) {
            int color = getFluid().getColor();
            float red = (color >> 16 & 0xFF) / 255.0f;
            float green = (color >> 8 & 0xFF) / 255.0f;
            float blue = (color & 0xFF) / 255.0f;
            return new Vec3d(red, green, blue);
        }

        //not inside fluid
        return originalColor;
    }

    /**
     * @reason expose definedFluid to significantly boost performance
     * @author jbred
     */
    @Nonnull
    @Overwrite(remap = false)
    public IBlockState getStateAtViewpoint(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Vec3d viewpoint) {
        if(isWithinFluid(world, pos, viewpoint.y)) return super.getStateAtViewpoint(state, world, pos, viewpoint);
        //return the other block here if the player isn't within the fluid
        final IBlockState here = world.getBlockState(pos);
        return here == state ? Blocks.AIR.getDefaultState()
                : here.getBlock().getStateAtViewpoint(here, world, pos, viewpoint);
    }

    @Shadow(remap = false)
    public abstract int getMaxRenderHeightMeta();

    @Shadow(remap = false)
    protected abstract int getFlowDecay(@Nonnull IBlockAccess world, @Nonnull BlockPos pos);

    @Shadow(remap = false)
    public abstract float getFilledPercentage(@Nonnull IBlockAccess world, @Nonnull BlockPos pos);

    @Nullable
    @Override
    public Boolean isEntityInsideMaterial(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState iblockstate, @Nonnull Entity entity, double yToTest, @Nonnull Material materialIn, boolean testingHead) {
        return materialIn != material ? null : isWithinFluid(world, pos, testingHead ? yToTest : entity.posY);
    }

    @Nullable
    @Override
    public Boolean isAABBInsideMaterial(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB boundingBox, @Nonnull Material materialIn) {
        return materialIn != material ? null : isAABBInsideLiquid(world, pos, boundingBox);
    }

    @Nullable
    @Override
    public Boolean isAABBInsideLiquid(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB boundingBox) {
        return isWithinFluid(world, pos, boundingBox.minY);
    }

    //forked from BlockFluidBase, but takes the y value directly
    private boolean isWithinFluid(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, double y) {
        final float filled = getFilledPercentage(world, pos);
        return filled < 0 ? y > pos.getY() + filled + 1
                : y < pos.getY() + filled;
    }
}
