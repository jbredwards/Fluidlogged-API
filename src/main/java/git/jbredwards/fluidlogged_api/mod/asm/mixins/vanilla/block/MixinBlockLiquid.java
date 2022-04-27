package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.ASMHooks;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@Mixin(BlockLiquid.class)
public abstract class MixinBlockLiquid extends Block implements IFluidloggableFluid
{
    public MixinBlockLiquid(@Nonnull Material materialIn) { super(materialIn); }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Nonnull
    @Overwrite
    protected BlockStateContainer createBlockState() {
        return new BlockStateContainer.Builder(this)
                .add(BlockFluidBase.FLUID_RENDER_PROPS.toArray(new IUnlistedProperty<?>[0]))
                .add(BlockLiquid.LEVEL).build();
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @SideOnly(Side.CLIENT)
    @Overwrite
    public boolean shouldSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState here = world.getBlockState(pos);
        if(!canFluidFlow(world, pos, here, side)) return true;

        final IBlockState neighbor = world.getBlockState(pos.offset(side));
        final boolean isCompatible = isCompatibleFluid(getFluidState(world, pos.offset(side), neighbor).getFluid(), getFluid());

        if(side == EnumFacing.UP) return !isCompatible || !canFluidFlow(world, pos.offset(side), neighbor, side.getOpposite());
        else return !isCompatible && super.shouldSideBeRendered(state, world, pos, side);
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Nonnull
    @Overwrite
    protected Vec3d getFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
        final int decay = 8 - getEffectiveQuanta(world, pos);
        Vec3d vec = Vec3d.ZERO;

        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            if(canFluidFlow(world, pos, here, facing)) {
                BlockPos offset = pos.offset(facing);

                if(canFluidFlow(world, offset, world.getBlockState(offset), facing.getOpposite())) {
                    int otherDecay = 8 - getEffectiveQuanta(world, offset);

                    if(otherDecay >= 8) {
                        otherDecay = 8 - getEffectiveQuanta(world, offset.down());

                        if(otherDecay < 8) {
                            int power = otherDecay - (decay - 8);
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

    protected int getEffectiveQuanta(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        int quantaValue = getQuantaValue(world, pos);
        return quantaValue > 0 && quantaValue < 8 && hasVerticalFlow(world, pos) ? 8 : quantaValue;
    }

    protected int getQuantaValue(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        if(!isCompatibleFluid(fluidState.getFluid(), getFluid())) return -1;

        final int level = fluidState.getLevel();
        return level >= 8 ? 8 : 8 - level;
    }

    protected boolean hasVerticalFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(!canFluidFlow(world, pos, world.getBlockState(pos), EnumFacing.UP)) return false;

        final IBlockState up = world.getBlockState(pos.up());
        return canFluidFlow(world, pos.up(), up, EnumFacing.DOWN)
                && isCompatibleFluid(getFluidState(world, pos.up(), up).getFluid(), getFluid());
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @SuppressWarnings("BooleanMethodIsAlwaysInverted")
    @Overwrite
    public boolean checkForMixing(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState stateIn) {
        if(stateIn.getMaterial() == Material.LAVA) {
            //get state here, since this method can also be executed from a FluidState
            final IBlockState here = worldIn.getBlockState(pos);
            if(here.getBlock().isReplaceable(worldIn, pos)) {
                for(EnumFacing facing : EnumFacing.values()) {
                    if(facing == EnumFacing.DOWN  || !canFluidFlow(worldIn, pos, here, facing))
                        continue;

                    BlockPos offset = pos.offset(facing);
                    IBlockState state = worldIn.getBlockState(offset);

                    if(!canFluidFlow(worldIn, offset, state, facing.getOpposite())) continue;
                    FluidState fluidState = getFluidState(worldIn, offset, state);

                    if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER) {
                        final int level = stateIn.getValue(BlockLiquid.LEVEL);

                        //obsidian
                        if(level == 0) {
                            worldIn.setBlockState(pos, ForgeEventFactory.fireFluidPlaceBlockEvent(worldIn, pos, pos, Blocks.OBSIDIAN.getDefaultState()));
                            triggerMixEffects(worldIn, pos);
                            return true;
                        }

                        //cobble
                        if(level <= 4 || facing == EnumFacing.UP) {
                            worldIn.setBlockState(pos, ForgeEventFactory.fireFluidPlaceBlockEvent(worldIn, pos, pos, Blocks.COBBLESTONE.getDefaultState()));
                            triggerMixEffects(worldIn, pos);
                            return true;
                        }

                        return false;
                    }
                }
            }
        }

        return false;
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite
    public static float getBlockLiquidHeight(@Nonnull IBlockState state, @Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) {
        final IBlockState up = worldIn.getBlockState(pos.up());
        final boolean flag = isCompatibleFluid(getFluidState(worldIn, pos.up(), up).getFluid(), getFluidFromState(state))
                && canFluidFlow(worldIn, pos.up(), up, EnumFacing.DOWN)
                && canFluidFlow(worldIn, pos, worldIn.getBlockState(pos), EnumFacing.UP);

        return flag ? 1 : 1 - (BlockLiquid.getLiquidHeightPercent(state.getValue(BlockLiquid.LEVEL)) - 1f/9);
    }

    @Nonnull
    @Override
    public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return ASMHooks.getFluidExtendedState(oldState, world, pos, getFluid(), -1, 8, 8f, 8f/9, getSlopeAngle(world, pos));
    }

    //fixes issue#59
    private float getSlopeAngle(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final Vec3d vec = getFlow(world, pos, world.getBlockState(pos));
        return vec.x == 0 && vec.z == 0 ? -1000 : (float)MathHelper.atan2(vec.z, vec.x) - (float)(Math.PI / 2);
    }

    @Nonnull
    @Override
    public Fluid getFluid() { return (material == Material.WATER) ? FluidRegistry.WATER : FluidRegistry.LAVA; }

    @Override
    public int place(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace) {
        if(fluidStack.amount < Fluid.BUCKET_VOLUME) return 0;
        if(doPlace) {
            final IBlockState here = world.getBlockState(pos);

            if(isStateFluidloggable(here, world, pos, getFluid())) {
                if(!setFluidState(world, pos, here, FluidState.of(this), true)) return 0; }
            else world.setBlockState(pos, BlockLiquid.getFlowingBlock(material).getDefaultState(), Constants.BlockFlags.DEFAULT_AND_RERENDER);
        }

        return Fluid.BUCKET_VOLUME;
    }

    @Nullable
    @Override
    public FluidStack drain(@Nonnull World world, @Nonnull BlockPos pos, boolean doDrain) {
        final IBlockState here = world.getBlockState(pos);
        final FluidState fluidState = getFluidState(world, pos, here);

        if(fluidState.isEmpty()) return null;
        if(doDrain) {
            if(fluidState.getState() == here) world.setBlockState(pos, Blocks.AIR.getDefaultState());
            else if(!setFluidState(world, pos, here, FluidState.EMPTY, false)) return null;
        }

        return fluidState.getLevel() != 0 ? null : new FluidStack(getFluid(), Fluid.BUCKET_VOLUME);
    }

    @Override
    public boolean canDrain(@Nonnull World world, @Nonnull BlockPos pos) { return getFluidState(world, pos).getLevel() == 0; }

    @Override
    public float getFilledPercentage(@Nonnull World world, @Nonnull BlockPos pos) {
        return getBlockLiquidHeight(getFluidOrReal(world, pos), world, pos);
    }

    @Override
    public boolean isFluidloggableFluid(@Nonnull IBlockState fluid, @Nonnull World world, @Nonnull BlockPos pos) {
        if(!isFluidloggableFluid()) return false;
        else if(fluid.getValue(BlockLiquid.LEVEL) == 0) return true;
        else if(fluid.getMaterial() != Material.WATER) return false;

        final IBlockState vertical = world.getBlockState(pos.up());
        return isCompatibleFluid(getFluid(), getFluidState(world, pos.up(), vertical).getFluid())
                && canFluidFlow(world, pos.up(), vertical, EnumFacing.DOWN);
    }

    @SuppressWarnings("ConstantConditions")
    @Override
    public boolean isFluidloggableFluid() {
        final Block block = this; //most modded BlockLiquid instances involve blocks that shouldn't be fluidloggable fluids (like coral)
        return block == Blocks.WATER || block == Blocks.LAVA || block == Blocks.FLOWING_WATER || block == Blocks.FLOWING_LAVA;
    }

    @Shadow
    protected abstract void triggerMixEffects(@Nonnull World worldIn, @Nonnull BlockPos pos);

    @Nullable
    @Override
    public Boolean isEntityInsideMaterial(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState iblockstate, @Nonnull Entity entity, double yToTest, @Nonnull Material materialIn, boolean testingHead) {
        return materialIn != material ? null : (testingHead ? yToTest : entity.posY) < pos.getY() + getBlockLiquidHeight(iblockstate, world, pos);
    }

    @Nullable
    @Override
    public Boolean isAABBInsideMaterial(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB boundingBox, @Nonnull Material materialIn) {
        return materialIn != material ? null : isAABBInsideLiquid(world, pos, boundingBox);
    }

    @Nullable
    @Override
    public Boolean isAABBInsideLiquid(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB boundingBox) {
        return boundingBox.minY < pos.getY() + getFilledPercentage(world, pos);
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
        if(ActiveRenderInfo.projectViewFromEntity(entity, partialTicks).y < pos.getY() + getBlockLiquidHeight(state, world, pos))
            return super.getFogColor(world, pos, state, entity, originalColor, partialTicks);

        //not inside fluid
        return originalColor;
    }

    @Nonnull
    @Override
    public IBlockState getStateAtViewpoint(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Vec3d viewpoint) {
        if(viewpoint.y < pos.getY() + getBlockLiquidHeight(state, world, pos)) return state;
        //return the other block here if the player isn't within the fluid
        final IBlockState here = world.getBlockState(pos);
        return here == state ? Blocks.AIR.getDefaultState()
                : here.getBlock().getStateAtViewpoint(here, world, pos, viewpoint);
    }
}
