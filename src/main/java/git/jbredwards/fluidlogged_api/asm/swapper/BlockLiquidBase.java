package git.jbredwards.fluidlogged_api.asm.swapper;

import git.jbredwards.fluidlogged_api.asm.ASMHooks;
import git.jbredwards.fluidlogged_api.common.block.AbstractFluidloggedBlock;
import git.jbredwards.fluidlogged_api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.BlockStairs;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;

/**
 * allows vanilla fluids to use the forge fluid rendering system
 * @author jbred
 *
 */
@SuppressWarnings("NullableProblems")
public abstract class BlockLiquidBase extends BlockLiquid
{
    protected BlockLiquidBase(Material materialIn) {
        super(materialIn);
    }

    @Override
    public IBlockState getExtendedState(IBlockState oldState, IBlockAccess world, BlockPos pos) {
        if(!(oldState instanceof IExtendedBlockState)) return oldState;
        //fluid var used later
        final Fluid fluid = (blockMaterial == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA);

        //covert to extended state
        IExtendedBlockState state = (IExtendedBlockState)oldState;
        state = state.withProperty(BlockFluidBase.FLOW_DIRECTION, (float)getFlowDirection(state, world, pos));

        //corner height variables
        final IBlockState[][] upBlockState = new IBlockState[3][3];
        final float[][] height = new float[3][3];
        final float[][] corner = new float[2][2];

        upBlockState[1][1] = world.getBlockState(pos.up());
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
                        upBlockState[i][j] = world.getBlockState(pos.add(i - 1, 0, j - 1).up());
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
        state = state.withProperty(BlockFluidBase.LEVEL_CORNERS[0], corner[0][0]);
        state = state.withProperty(BlockFluidBase.LEVEL_CORNERS[1], corner[0][1]);
        state = state.withProperty(BlockFluidBase.LEVEL_CORNERS[2], corner[1][1]);
        state = state.withProperty(BlockFluidBase.LEVEL_CORNERS[3], corner[1][0]);
        return state;
    }

    public double getFlowDirection(IBlockState state, IBlockAccess world, BlockPos pos) {
        if(!state.getMaterial().isLiquid()) return -1000;

        final Vec3d vec = getFlow(world, pos, state);
        return vec.x == 0 && vec.z == 0 ? -1000 : Math.atan2(vec.z, vec.x) - Math.PI / 2;
    }

    @Override
    public EnumBlockRenderType getRenderType(IBlockState state) {
        return EnumBlockRenderType.MODEL;
    }

    //==================================================
    //BELOW FIXES FLUID BEHAVIOR WITH FLUIDLOGGED BLOCKS
    //==================================================

    @Override
    protected boolean causesDownwardCurrent(IBlockAccess worldIn, BlockPos pos, EnumFacing side) {
        final Fluid fluid = (blockMaterial == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA);
        final IBlockState state = worldIn.getBlockState(pos);

        if(FluidloggedUtils.getFluidFromBlock(state.getBlock()) == fluid) return false;
        else if(side == EnumFacing.UP) return true;
        else if(state.getMaterial() == Material.ICE) return false;
        else {
            final Block block = state.getBlock();
            boolean flag = isExceptBlockForAttachWithPiston(block) || block instanceof BlockStairs;
            return !flag && state.getBlockFaceShape(worldIn, pos, side) == BlockFaceShape.SOLID;
        }
    }

    @Override
    protected Vec3d getFlow(IBlockAccess world, BlockPos pos, IBlockState here) {
        Vec3d vec = new Vec3d(0, 0, 0);
        final int level = here.getValue(LEVEL);
        int decay = (level >= 8 ? 0 : level);

        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            BlockPos offset = pos.offset(facing);
            int otherDecay = getFlowDecay(world.getBlockState(offset), world, offset, facing);
            if(otherDecay < 0) {
                if(!ASMHooks.getFlow(world.getBlockState(offset), world, pos, facing, this)) {
                    otherDecay = getFlowDecay(world.getBlockState(offset.down()), world, offset.down(), facing);
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
                if(causesDownwardCurrent(world, offset, side) || causesDownwardCurrent(world, offset.up(), side)) {
                    vec = vec.normalize().addVector(0.0, -6.0, 0.0);
                    break;
                }
            }
        }

        return vec.normalize();
    }

    public int getFlowDecay(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing facing) {
        if(FluidloggedUtils.getFluidFromBlock(this) != FluidloggedUtils.getFluidFromBlock(state.getBlock())) return -1;
        else if(state.getBlock() instanceof AbstractFluidloggedBlock && !((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, pos, facing.getOpposite())) return -1;
        else {
            final int level = state.getValue(LEVEL);
            return level >= 8 ? 0 : level;
        }
    }

    @Override
    public boolean checkForMixing(World worldIn, BlockPos pos, IBlockState here) {
        if(blockMaterial == Material.LAVA) {
            boolean flag = false;

            for(EnumFacing facing : EnumFacing.values()) {
                final BlockPos offset = pos.offset(facing);
                final IBlockState state = worldIn.getBlockState(offset);

                if(state.getMaterial() == Material.WATER && (!(state.getBlock() instanceof AbstractFluidloggedBlock) || ((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, worldIn, offset, facing.getOpposite()))) {
                    flag = true;
                    break;
                }
            }

            if(flag) {
                int level = here.getValue(LEVEL);

                if(level == 0) {
                    worldIn.setBlockState(pos, ForgeEventFactory.fireFluidPlaceBlockEvent(worldIn, pos, pos, Blocks.OBSIDIAN.getDefaultState()));
                    triggerMixEffects(worldIn, pos);
                    return true;
                }

                if(level <= 4) {
                    worldIn.setBlockState(pos, ForgeEventFactory.fireFluidPlaceBlockEvent(worldIn, pos, pos, Blocks.COBBLESTONE.getDefaultState()));
                    triggerMixEffects(worldIn, pos);
                    return true;
                }
            }
        }

        return false;
    }
}
