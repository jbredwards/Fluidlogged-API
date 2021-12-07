package git.jbredwards.fluidlogged_api.asm.replacements;

import git.jbredwards.fluidlogged_api.asm.plugins.ASMHooks;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggableBase;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.BlockStairs;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;

import static net.minecraft.util.EnumFacing.*;

/**
 * -allows BlockLiquid to render through the ModelFluid renderer by providing the necessary properties
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
        pos = pos.offset(side);
        final IBlockState neighbor = blockAccess.getBlockState(pos);
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidAt(blockAccess, pos, neighbor);

        if(fluid == FluidloggedUtils.getFluidFromBlock(this)) return false;
        else return side == EnumFacing.UP || !neighbor.doesSideBlockRendering(blockAccess, pos, side.getOpposite());
    }

    @Override
    public boolean isReplaceable(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) { return true; }

    @Override
    public boolean causesDownwardCurrent(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState state = worldIn.getBlockState(pos);
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidAt(worldIn, pos, state);

        if(fluid == (blockMaterial == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA)
                && IFluidloggableBase.canFluidFlow(worldIn, pos, state, fluid, side)) return false;

        else if(side == UP) return true;
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
        //correct state param sometimes not being a BlockLiquid
        if(FluidloggedUtils.getFluidFromState(state) == null) {
            FluidState fluidState = FluidState.get(worldIn, pos);
            if(fluidState.isEmpty()) return Vec3d.ZERO;
            else state = fluidState.getState();
        }

        Vec3d vec = new Vec3d(0, 0, 0);

        final Fluid fluid = (blockMaterial == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA);
        final int level = state.getValue(LEVEL);
        int decay = (level >= 8 ? 0 : level);

        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            BlockPos offset = pos.offset(facing);
            int otherDecay = getFlowDecay(FluidloggedUtils.getFluidOrReal(worldIn, offset), worldIn, offset, facing);
            if(otherDecay < 0) {
                if(!getFlow(worldIn, offset, worldIn.getBlockState(offset), facing, fluid)) {
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

    protected boolean getFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull EnumFacing facing, @Nonnull Fluid fluidIn) {
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidAt(world, pos, state);
        //fluidlogged block
        if(fluid == fluidIn && !IFluidloggableBase.canFluidFlow(world, pos, state, fluid, facing.getOpposite())) return true;
        //fluids aren't equal
        else if(fluid != null && fluid != fluidIn) return true;
        //default
        return state.getMaterial().blocksMovement();
    }

    @Override
    public boolean checkForMixing(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState stateIn) {
        if(blockMaterial == Material.LAVA) {
            //get state here, since this method can also be executed from a FluidState
            final IBlockState here = worldIn.getBlockState(pos);
            if(here.getBlock().isReplaceable(worldIn, pos)) {
                for(EnumFacing facing : EnumFacing.values()) {
                    if(facing == DOWN || !IFluidloggableBase.canFluidFlow(worldIn, pos, here, FluidRegistry.LAVA, facing)) continue;

                    BlockPos offset = pos.offset(facing);
                    IBlockState state = worldIn.getBlockState(offset);
                    @Nullable Fluid fluid = FluidloggedUtils.getFluidAt(worldIn, offset, state);

                    if(fluid != null && fluid.getBlock().getDefaultState().getMaterial() == Material.WATER && IFluidloggableBase.canFluidFlow(worldIn, offset, state, fluid, facing.getOpposite())) {
                        final int level = stateIn.getValue(LEVEL);

                        //obsidian
                        if(level == 0) {
                            worldIn.setBlockState(pos, ForgeEventFactory.fireFluidPlaceBlockEvent(worldIn, pos, pos, Blocks.OBSIDIAN.getDefaultState()));
                            triggerMixEffects(worldIn, pos);
                            return true;
                        }

                        //cobble
                        if(level <= 4) {
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
        final Fluid[][] upFluid = new Fluid[3][3];
        final float[][] height = new float[3][3];
        final float[][] corner = new float[2][2];

        upBlockState[1][1] = world.getBlockState(pos.up());
        upFluid[1][1] = FluidloggedUtils.getFluidAt(world, pos.up(), upBlockState[1][1]);
        height[1][1] = getFluidHeightForRender(fluid, world, pos, upBlockState[1][1], upFluid[1][1], 1, 1);

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
                        upFluid[i][j] = FluidloggedUtils.getFluidAt(world, pos.add(i - 1, 0, j - 1).up(), upBlockState[i][j]);
                        height[i][j] = getFluidHeightForRender(fluid, world, pos.add(i - 1, 0, j - 1), upBlockState[i][j], upFluid[i][j], i, j);
                    }
                }
            }
            //find average of all heights for each corner
            for(int i = 0; i < 2; i++) {
                for(int j = 0; j < 2; j++) {
                    corner[i][j] = getFluidHeightAverage(8f/9, i, j, height[i][j], height[i][j + 1], height[i + 1][j], height[i + 1][j + 1]);
                }
            }

            //check for downflow above corners
            boolean n =  isFluid(upBlockState[0][1], upFluid[0][1], fluid, world, pos.north(), NORTH);
            boolean s =  isFluid(upBlockState[2][1], upFluid[2][1], fluid, world, pos.south(), SOUTH);
            boolean w =  isFluid(upBlockState[1][0], upFluid[1][0], fluid, world, pos.west(),  WEST);
            boolean e =  isFluid(upBlockState[1][2], upFluid[1][2], fluid, world, pos.east(),  EAST);
            boolean nw = isFluid(upBlockState[0][0], upFluid[0][0], fluid, world, pos.north().west(), NORTH, WEST);
            boolean ne = isFluid(upBlockState[0][2], upFluid[0][2], fluid, world, pos.north().east(), NORTH, EAST);
            boolean sw = isFluid(upBlockState[2][0], upFluid[2][0], fluid, world, pos.south().west(), SOUTH, WEST);
            boolean se = isFluid(upBlockState[2][2], upFluid[2][2], fluid, world, pos.south().east(), SOUTH, EAST);
            if(nw || n || w) corner[0][0] = 1;
            if(ne || n || e) corner[0][1] = 1;
            if(sw || s || w) corner[1][0] = 1;
            if(se || s || e) corner[1][1] = 1;
        }

        //side overlays
        for(int i = 0; i < 4; i++) {
            EnumFacing side = EnumFacing.getHorizontal(i);
            BlockPos offset = pos.offset(side);
            boolean useOverlay = world.getBlockState(offset).getBlockFaceShape(world, offset, side.getOpposite()) == BlockFaceShape.SOLID;
            state = state.withProperty(BlockFluidBase.SIDE_OVERLAYS[i], useOverlay);
        }

        //sets the corner props
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[0], corner[0][0], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[1], corner[0][1], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[2], corner[1][1], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[3], corner[1][0], 8f/9);
        return state;
    }

    //used by getExtendedState
    public float getFluidHeightForRender(Fluid fluid, IBlockAccess world, BlockPos pos, IBlockState up, Fluid upFluid, int i, int j) {
        IBlockState here = world.getBlockState(pos);
        if (here.getMaterial().isLiquid() && here.getMaterial() == up.getMaterial()) {
            return 1;
        }

        if (here.getBlock() == this && here.getBlock().getMetaFromState(here) == 0) {
            return 8f/9;
        }

        if (here.getBlock() instanceof BlockLiquid) {
            return Math.min(1 - BlockLiquid.getLiquidHeightPercent(here.getValue(BlockLiquid.LEVEL)), 8f/9);
        }

        return (here.getBlock().isAir(here, world, pos) ? 0 : (-8f/9));

        //check block above
        /*if(isFluid(up, upFluid, fluid, world, pos.up(), UP)) return 1;
        final IBlockState state = world.getBlockState(pos);

        //is air
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final boolean canSideFlow = ASMHooks.canSideFlow(fluid, state, world, pos, i, j);
        final boolean fluidMatches = (FluidloggedUtils.getFluidAt(world, pos, state) == fluid);

        if(fluidMatches && canSideFlow) {
            if(state.getValue(BlockLiquid.LEVEL) == 0) return 8f / 9;
        }

        //not fluid
        if(!fluidMatches || !canSideFlow) return (-1 / 8f) * (8f / 9);
        //fluid
        else return ((8 - state.getValue(BlockLiquid.LEVEL)) / 8f) * (8f / 9);*/
    }

    //used by getExtendedState
    public float getFluidHeightAverage(float quantaFraction, int i, int j, float... flow) {
        float total = 0;
        int count = 0;

        for(int index = 0; index < flow.length; index++) {
            //fix corners visually flowing into illegal sides (vanilla 1.13 bug)
            if(ASMHooks.fixN[i] && j == 1 && index % 2 == 1) continue;
            if(ASMHooks.fixS[i] && j == 0 && index % 2 == 0) continue;
            if(ASMHooks.fixE[j] && i == 0 && index < 2) continue;
            if(ASMHooks.fixW[j] && i == 1 && index > 1) continue;

            //old
            if(flow[index] >= quantaFraction) {
                total += flow[index] * 10;
                count += 10;
            }

            if(flow[index] >= 0) {
                total += flow[index];
                count++;
            }
        }

        return total / count;
    }

    //used by getExtendedState
    public boolean isFluid(IBlockState up, Fluid upFluid, Fluid fluid, IBlockAccess world, BlockPos pos, EnumFacing...faces) {
        if(fluid == upFluid) {
            for(EnumFacing facing : faces) {
                if(!IFluidloggableBase.canFluidFlow(world, pos, up, upFluid, facing.getOpposite())) return false;
            }

            return true;
        }

        return false;
    }

    //fixes crash when trying to render invalid state (temporary fix)
    public <V> IExtendedBlockState withPropertyFallback(IExtendedBlockState state, IUnlistedProperty<V> property, @Nullable V value, V fallback) {
        return state.withProperty(property, property.isValid(value) ? value : fallback);
    }

    public double getFlowDirection(IBlockState state, IBlockAccess world, BlockPos pos) {
        final Vec3d vec = getFlow(world, pos, state);
        return vec.x == 0 && vec.z == 0 ? -1000 : Math.atan2(vec.z, vec.x) - Math.PI / 2;
    }

    public int getFlowDecay(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing facing) {
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidAt(world, pos, state);

        if(FluidloggedUtils.getFluidFromBlock(this) != fluid) return -1;
        else if(!IFluidloggableBase.canFluidFlow(world, pos, state, fluid, facing.getOpposite())) return -1;
        else {
            final int level = state.getValue(LEVEL);
            return level >= 8 ? 0 : level;
        }
    }

    protected static Map<Block, Boolean> defaultDisplacements = null;
    protected static Map<Block, Boolean> getDefaultDisplacements() {
        if(defaultDisplacements == null) defaultDisplacements = ASMHooks.defaultDisplacements(new HashMap<>());
        return defaultDisplacements;
    }

    //static version of BlockFluidBase#canDisplace
    public static boolean canDisplace(@Nonnull Block fluidBlock, @Nonnull Fluid fluidIn, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        final Block block = state.getBlock();

        if(block.isAir(state, world, pos)) return true;
        else if(ASMHooks.canDisplace(state, fluidBlock, fluidIn, world, pos) == fluidBlock) return false;
        else if(getDefaultDisplacements().containsKey(block)) return defaultDisplacements.get(block);

        final Material material = state.getMaterial();
        if(material.blocksMovement() || material == Material.PORTAL || material == Material.STRUCTURE_VOID) {
            return false;
        }

        final boolean replaceable = block.isReplaceable(world, pos) || FluidloggedUtils.isStateFluidloggable(state, fluidIn);
        final int density = BlockFluidBase.getDensity(world, pos);

        if(density == Integer.MAX_VALUE) return replaceable;
        else return replaceable && 1000 > density;
    }
}
