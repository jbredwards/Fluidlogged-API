package git.jbredwards.fluidlogged_api.asm.replacements;

import git.jbredwards.fluidlogged_api.asm.ASMHooks;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggableBase;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.BlockStairs;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * -allows vanilla fluid blocks to render through the forge fluid renderer by providing the necessary properties
 * -all classes that extend BlockLiquid will extend this class instead during runtime automatically, through asm
 * @author jbred
 *
 */
@SuppressWarnings("unused")
public abstract class BlockLiquidBase extends BlockLiquid
{
    protected BlockLiquidBase(Material materialIn) { super(materialIn); }

    @Nonnull
    @Override
    protected BlockStateContainer createBlockState() {
        return new BlockStateContainer.Builder(this).add(LEVEL).add(BlockFluidBase.FLUID_RENDER_PROPS.toArray(new IUnlistedProperty<?>[0])).build();
    }

    @Nonnull
    @Override
    public EnumBlockRenderType getRenderType(@Nonnull IBlockState state) {
        return EnumBlockRenderType.MODEL;
    }

    @SideOnly(Side.CLIENT)
    @Nonnull
    @Override
    public BlockRenderLayer getBlockLayer() {
        return blockMaterial == Material.WATER ? BlockRenderLayer.TRANSLUCENT : BlockRenderLayer.SOLID;
    }

    @Override
    public boolean causesDownwardCurrent(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final Fluid fluid = (blockMaterial == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA);
        final IBlockState state = FluidloggedUtils.getFluidOrReal(worldIn, pos);

        if(FluidloggedUtils.getFluidFromBlock(state.getBlock()) == fluid) return false;
        else if(side == EnumFacing.UP) return true;
        else if(state.getMaterial() == Material.ICE) return false;
        else {
            final Block block = state.getBlock();
            boolean flag = isExceptBlockForAttachWithPiston(block) || block instanceof BlockStairs;
            return !flag && state.getBlockFaceShape(worldIn, pos, side) == BlockFaceShape.SOLID;
        }
    }

    @Nonnull
    @Override
    protected Vec3d getFlow(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        Vec3d vec = new Vec3d(0, 0, 0);
        final int level = state.getValue(LEVEL);
        int decay = (level >= 8 ? 0 : level);

        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            BlockPos offset = pos.offset(facing);
            state = FluidloggedUtils.getFluidOrReal(worldIn, offset);
            int otherDecay = getFlowDecay(state, worldIn, offset, facing);
            if(otherDecay < 0) {
                if(!ASMHooks.getFlow(state, worldIn, pos, facing, this)) {
                    otherDecay = getFlowDecay(FluidloggedUtils.getFluidOrReal(worldIn, offset.down()), worldIn, offset.down(), facing);
                    if(otherDecay >= 0) {
                        int power = otherDecay - (decay - 8);
                        vec = vec.addVector(facing.getFrontOffsetX() * power, 0, facing.getFrontOffsetZ() * power);
                    }
                }
            }
            else {
                int power = otherDecay - decay;
                vec = vec.addVector(facing.getFrontOffsetX() * power, 0, facing.getFrontOffsetZ() * power);
            }
        }

        if(level >= 8) {
            for(EnumFacing side : EnumFacing.HORIZONTALS) {
                BlockPos offset = pos.offset(side);
                if(causesDownwardCurrent(worldIn, offset, side) || causesDownwardCurrent(worldIn, offset.up(), side)) {
                    vec = vec.normalize().addVector(0.0, -6.0, 0.0);
                    break;
                }
            }
        }

        return vec.normalize();
    }

    @Override
    public boolean checkForMixing(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        return super.checkForMixing(worldIn, pos, state);
    }

    @Nonnull
    @Override
    public IBlockState getExtendedState(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return super.getExtendedState(state, world, pos);
    }

    public double getFlowDirection(IBlockState state, IBlockAccess world, BlockPos pos) {
        if(!state.getMaterial().isLiquid()) return -1000;

        final Vec3d vec = getFlow(world, pos, state);
        return vec.x == 0 && vec.z == 0 ? -1000 : Math.atan2(vec.z, vec.x) - Math.PI / 2;
    }

    public int getFlowDecay(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing facing) {
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(state.getBlock());

        if(FluidloggedUtils.getFluidFromBlock(this) != fluid) return -1;
        else if(!IFluidloggableBase.canFluidFlow(world, pos, state, fluid, facing.getOpposite())) return -1;
        else {
            final int level = state.getValue(LEVEL);
            return level >= 8 ? 0 : level;
        }
    }
}
