package git.jbredwards.fluidlogged_api.asm.replacements;

import git.jbredwards.fluidlogged_api.asm.plugins.ASMHooks;
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
import net.minecraftforge.common.property.IExtendedBlockState;
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
public abstract class BlockLiquidBase extends BlockLiquid
{
    protected BlockLiquidBase(Material materialIn) { super(materialIn); }

    @Nonnull
    @Override
    protected BlockStateContainer createBlockState() {
        return new BlockStateContainer.Builder(this)
                .add(LEVEL)
                .add(BlockFluidBase.FLUID_RENDER_PROPS.toArray(new IUnlistedProperty<?>[0]))
                .build();
    }

    @Nonnull
    @Override
    public EnumBlockRenderType getRenderType(@Nonnull IBlockState state) { return EnumBlockRenderType.MODEL; }

    @SideOnly(Side.CLIENT)
    @Nonnull
    @Override
    public BlockRenderLayer getBlockLayer() {
        return blockMaterial == Material.WATER ? BlockRenderLayer.TRANSLUCENT : BlockRenderLayer.SOLID;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public boolean shouldSideBeRendered(@Nonnull IBlockState blockState, @Nonnull IBlockAccess blockAccess, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        if(FluidloggedUtils.getFluidFromBlock(FluidloggedUtils.getFluidOrReal(blockAccess, pos.offset(side)).getBlock()) == FluidloggedUtils.getFluidFromBlock(this)) return false;
        else return side == EnumFacing.UP || !blockAccess.getBlockState(pos.offset(side)).doesSideBlockRendering(blockAccess, pos.offset(side), side.getOpposite());
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
    public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(!(oldState instanceof IExtendedBlockState)) return oldState;
        //fluid var used later
        final Fluid fluid = (blockMaterial == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA);

        //covert to extended state
        IExtendedBlockState state = (IExtendedBlockState)oldState;
        state = state.withProperty(BlockFluidBase.FLOW_DIRECTION, (float)getFlowDirection(oldState, world, pos));

        //corner height variables
        final IBlockState[][] upBlockState = new IBlockState[3][3];
        final float[][] height = new float[3][3];
        final float[][] corner = new float[2][2];

        upBlockState[1][1] = FluidloggedUtils.getFluidOrReal(world, pos.up());
        height[1][1] = ASMHooks.getFluidHeightForRender(fluid, world, pos, upBlockState[1][1], 1, 1);

        //fluid block above this
        if(height[1][1] == 1) {
            for(int i = 0; i < 2; i++) {
                for(int j = 0; j < 2; j++) {
                    corner[i][j] = 1;
                }
            }
        }
        //no fluid block above this
        else {
            //get corner heights from all 8 sides
            for(int i = 0; i < 3; i++) {
                for(int j = 0; j < 3; j++) {
                    if(i != 1 || j != 1) {
                        upBlockState[i][j] = FluidloggedUtils.getFluidOrReal(world, pos.add(i - 1, 0, j - 1).up());
                        height[i][j] = ASMHooks.getFluidHeightForRender(fluid, world, pos.add(i - 1, 0, j - 1), upBlockState[i][j], i, j);
                    }
                }
            }
            //find average of all heights for each corner
            for(int i = 0; i < 2; i++) {
                for(int j = 0; j < 2; j++) {
                    corner[i][j] = ASMHooks.getFluidHeightAverage(8f/9, i, j, height[i][j], height[i][j + 1], height[i + 1][j], height[i + 1][j + 1]);
                }
            }

            //check for downflow above corners
            boolean n =  ASMHooks.isFluid(upBlockState[0][1], fluid, world, pos.north());
            boolean s =  ASMHooks.isFluid(upBlockState[2][1], fluid, world, pos.south());
            boolean w =  ASMHooks.isFluid(upBlockState[1][0], fluid, world, pos.west());
            boolean e =  ASMHooks.isFluid(upBlockState[1][2], fluid, world, pos.east());
            boolean nw = ASMHooks.isFluid(upBlockState[0][0], fluid, world, pos.north().west());
            boolean ne = ASMHooks.isFluid(upBlockState[0][2], fluid, world, pos.north().east());
            boolean sw = ASMHooks.isFluid(upBlockState[2][0], fluid, world, pos.south().west());
            boolean se = ASMHooks.isFluid(upBlockState[2][2], fluid, world, pos.south().east());
            if(nw || n || w) corner[0][0] = 1;
            if(ne || n || e) corner[0][1] = 1;
            if(sw || s || w) corner[1][0] = 1;
            if(se || s || e) corner[1][1] = 1;
        }

        //side overlays
        for(int i = 0; i < 4; i++) {
            EnumFacing facing = EnumFacing.getHorizontal(i);
            BlockPos offset = pos.offset(facing);
            boolean useOverlay = world.getBlockState(offset).getBlockFaceShape(world, offset, facing.getOpposite()) == BlockFaceShape.SOLID;
            state = state.withProperty(BlockFluidBase.SIDE_OVERLAYS[i], useOverlay);
        }

        //sets the corner props
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[0], corner[0][0], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[1], corner[0][1], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[2], corner[1][1], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[3], corner[1][0], 8f/9);
        return state;
    }

    //fixes crash when trying to render invalid state (temporary fix)
    public <V> IExtendedBlockState withPropertyFallback(IExtendedBlockState state, IUnlistedProperty<V> property, @Nullable V value, V fallback) {
        return state.withProperty(property, property.isValid(value) ? value : fallback);
    }

    public static double getFlowDirection(IBlockState state, IBlockAccess world, BlockPos pos) {
        if(!state.getMaterial().isLiquid()) return -1000;

        final Vec3d vec = state.getBlock() instanceof BlockFluidBase ? ((BlockFluidBase)state.getBlock()).getFlowVector(world, pos):
                state.getBlock() instanceof BlockLiquidBase ? ((BlockLiquidBase)state.getBlock()).getFlow(world, pos, state) : Vec3d.ZERO;

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
