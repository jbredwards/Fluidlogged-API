package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.asm.plugins.ASMHooks;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.BlockStairs;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockLiquid.class)
public abstract class BlockLiquidMixin extends Block implements IFluidBlock, IFluidloggableFluid
{
    public BlockLiquidMixin(@Nonnull Material materialIn) { super(materialIn); }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Nonnull
    @Overwrite
    protected BlockStateContainer createBlockState() {
        return new BlockStateContainer.Builder(this)
                .add(BlockLiquid.LEVEL)
                .add(BlockFluidBase.FLUID_RENDER_PROPS.toArray(new IUnlistedProperty<?>[0]))
                .build();
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Nonnull
    @Overwrite
    public EnumBlockRenderType getRenderType(@Nonnull IBlockState state) { return EnumBlockRenderType.MODEL; }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @SideOnly(Side.CLIENT)
    @Overwrite
    public boolean shouldSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState here = world.getBlockState(pos);

        if(!canFluidFlow(world, pos, here, side))
            return !here.doesSideBlockRendering(world, pos, side) && here.getRenderType() != EnumBlockRenderType.INVISIBLE;

        else {
            final BlockPos offset = pos.offset(side);
            final IBlockState neighbor = world.getBlockState(offset);

            if(isCompatibleFluid(getFluidState(world, offset, neighbor).getFluid(), getFluidFromState(state)))
                if(!canFluidFlow(world, offset, neighbor, side.getOpposite()))
                    return !neighbor.doesSideBlockRendering(world, offset, side.getOpposite()) && neighbor.getRenderType() != EnumBlockRenderType.INVISIBLE;

                else return false;

            else return side == EnumFacing.UP || !neighbor.doesSideBlockRendering(world, offset, side.getOpposite());
        }
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    private boolean causesDownwardCurrent(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState state = worldIn.getBlockState(pos);
        if(isCompatibleFluid(getFluidState(worldIn, pos, state).getFluid(), getFluid())
                && canFluidFlow(worldIn, pos, state, side)) return false;

        else if(side == EnumFacing.UP) return true;
        else if(state.getMaterial() == Material.ICE) return false;
        else {
            final Block block = state.getBlock();
            boolean flag = isExceptBlockForAttachWithPiston(block) || block instanceof BlockStairs;
            return !flag && state.getBlockFaceShape(worldIn, pos, side) == BlockFaceShape.SOLID;
        }
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Nonnull
    @Overwrite
    protected Vec3d getFlow(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        //correct state param sometimes not being a BlockLiquid
        if(getFluidFromState(state) == null) {
            FluidState fluidState = FluidState.get(worldIn, pos);
            if(fluidState.isEmpty()) return Vec3d.ZERO;
            else state = fluidState.getState();
        }

        Vec3d vec = new Vec3d(0, 0, 0);

        final IBlockState here = worldIn.getBlockState(pos);
        final int level = state.getValue(BlockLiquid.LEVEL);
        int decay = (level >= 8 ? 0 : level);

        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            if(canFluidFlow(worldIn, pos, here, facing)) {
                BlockPos offset = pos.offset(facing);
                IBlockState neighbor = worldIn.getBlockState(offset);
                int otherDecay = getFlowDecay(neighbor, worldIn, offset, facing);
                if(otherDecay < 0) {
                    if(!getFlow(worldIn, offset, neighbor, facing, getFluid())) {
                        otherDecay = getFlowDecay(worldIn.getBlockState(offset.down()), worldIn, offset.down(), facing);
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
        }

        if(level >= 8) {
            for(EnumFacing side : EnumFacing.HORIZONTALS) {
                if(canFluidFlow(worldIn, pos, here, side)) {
                    BlockPos offset = pos.offset(side);
                    if(causesDownwardCurrent(worldIn, offset, side) || causesDownwardCurrent(worldIn, offset.up(), side)) {
                        vec = vec.normalize().addVector(0.0, -6.0, 0.0);
                        break;
                    }
                }
            }
        }

        return vec.normalize();
    }

    protected int getFlowDecay(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing facing) {
        final @Nullable Fluid fluid = getFluidState(world, pos, state).getFluid();

        if(!isCompatibleFluid(getFluid(), fluid)) return -1;
        else if(!canFluidFlow(world, pos, state, facing.getOpposite())) return -1;
        else {
            final int level = getFluidOrReal(world, pos, state).getValue(BlockLiquid.LEVEL);
            return level >= 8 ? 0 : level;
        }
    }

    protected boolean getFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull EnumFacing facing, @Nonnull Fluid fluidIn) {
        final @Nullable Fluid fluid = getFluidState(world, pos, state).getFluid();
        //fluidlogged block
        if(isCompatibleFluid(fluid, fluidIn) && !canFluidFlow(world, pos, state, facing.getOpposite())) return true;
        //fluids aren't equal
        else if(fluid != null && !isCompatibleFluid(fluid, fluidIn)) return true;
        //default
        return state.getMaterial().blocksMovement();
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    public boolean checkForMixing(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState stateIn) {
        if(blockMaterial == Material.LAVA) {
            //get state here, since this method can also be executed from a FluidState
            final IBlockState here = worldIn.getBlockState(pos);
            if(here.getBlock().isReplaceable(worldIn, pos)) {
                for(EnumFacing facing : EnumFacing.values()) {
                    if(facing == EnumFacing.DOWN || !canFluidFlow(worldIn, pos, here, facing)) continue;

                    BlockPos offset = pos.offset(facing);
                    IBlockState state = worldIn.getBlockState(offset);
                    FluidState fluidState = getFluidState(worldIn, offset, state);

                    if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER && canFluidFlow(worldIn, offset, state, facing.getOpposite())) {
                        final int level = stateIn.getValue(BlockLiquid.LEVEL);

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

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    public static float getBlockLiquidHeight(@Nonnull IBlockState state, @Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) {
        final FluidState fluidState = getFluidState(worldIn, pos, state);
        if(fluidState.isEmpty()) return 1;

        final IBlockState up = worldIn.getBlockState(pos.up());
        final int level = fluidState.getLevel();
        final boolean flag = (level & 7) == 0
                && isCompatibleFluid(getFluidState(worldIn, pos.up(), up).getFluid(), fluidState.getFluid())
                && canFluidFlow(worldIn, pos.up(), up, EnumFacing.DOWN);

        return flag ? 1 : 1 - BlockLiquid.getLiquidHeightPercent(level);
    }

    @Nonnull
    @Redirect(method = "getFogColor", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFogColor(@Nonnull World world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Shadow
    protected abstract void triggerMixEffects(@Nonnull World worldIn, @Nonnull BlockPos pos);

    @Nonnull
    @Override
    public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(!(oldState instanceof IExtendedBlockState)) return oldState;
        //fluid var used later
        final Fluid fluid = getFluid();

        //covert to extended state
        IExtendedBlockState state = (IExtendedBlockState)oldState;
        state = state.withProperty(BlockFluidBase.FLOW_DIRECTION, (float)getFlowDirection(oldState, world, pos));

        //corner height variables
        final IBlockState[][] upBlockState = new IBlockState[3][3];
        final Fluid[][] upFluid = new Fluid[3][3];
        final float[][] height = new float[3][3];
        final float[][] corner = new float[2][2];

        upBlockState[1][1] = world.getBlockState(pos.up());
        upFluid[1][1] = getFluidState(world, pos.up(), upBlockState[1][1]).getFluid();
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
                        upFluid[i][j] = getFluidState(world, pos.add(i - 1, 0, j - 1).up(), upBlockState[i][j]).getFluid();
                        height[i][j] = getFluidHeightForRender(fluid, world, pos.add(i - 1, 0, j - 1), upBlockState[i][j], upFluid[i][j], i, j);
                    }
                }
            }
            //find average of all heights for each corner
            for(int i = 0; i < 2; i++) {
                for(int j = 0; j < 2; j++) {
                    corner[i][j] = getFluidHeightAverage(i, j, height[i][j], height[i][j + 1], height[i + 1][j], height[i + 1][j + 1]);
                }
            }

            //check for downflow above corners
            boolean n =  isFluid(upBlockState[0][1], upFluid[0][1], fluid, world, pos.north(), EnumFacing.NORTH);
            boolean s =  isFluid(upBlockState[2][1], upFluid[2][1], fluid, world, pos.south(), EnumFacing.SOUTH);
            boolean w =  isFluid(upBlockState[1][0], upFluid[1][0], fluid, world, pos.west(),  EnumFacing.WEST);
            boolean e =  isFluid(upBlockState[1][2], upFluid[1][2], fluid, world, pos.east(),  EnumFacing.EAST);
            boolean nw = isFluid(upBlockState[0][0], upFluid[0][0], fluid, world, pos.north().west(), EnumFacing.NORTH, EnumFacing.WEST);
            boolean ne = isFluid(upBlockState[0][2], upFluid[0][2], fluid, world, pos.north().east(), EnumFacing.NORTH, EnumFacing.EAST);
            boolean sw = isFluid(upBlockState[2][0], upFluid[2][0], fluid, world, pos.south().west(), EnumFacing.SOUTH, EnumFacing.WEST);
            boolean se = isFluid(upBlockState[2][2], upFluid[2][2], fluid, world, pos.south().east(), EnumFacing.SOUTH, EnumFacing.EAST);
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
    public float getFluidHeightForRender(@Nullable Fluid fluid, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState up, @Nullable Fluid upFluid, int i, int j) {
        //check block above
        if(isFluid(up, upFluid, fluid, world, pos.up(), EnumFacing.UP)) return 1;
        final IBlockState state = world.getBlockState(pos);

        //is air
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final boolean canSideFlow = ASMHooks.canSideFlow(fluid, state, world, pos, i, j);
        final boolean fluidMatches = isCompatibleFluid(getFluidState(world, pos, state).getFluid(), fluid);

        if(fluidMatches && canSideFlow) {
            if(getFluidOrReal(world, pos, state).getValue(BlockLiquid.LEVEL) == 0) return 8f / 9;
        }

        //not fluid
        if(!fluidMatches || !canSideFlow) return (-1 / 8f) * (8f / 9);
        //fluid
        else return ((8 - getFluidOrReal(world, pos, state).getValue(BlockLiquid.LEVEL)) / 8f) * (8f / 9);
    }

    //used by getExtendedState
    public float getFluidHeightAverage(int i, int j, @Nonnull float... flow) {
        float total = 0;
        int count = 0;

        for(int index = 0; index < flow.length; index++) {
            //fix corners visually flowing into illegal sides (vanilla 1.13 bug)
            if(ASMHooks.fixN[i] && j == 1 && index % 2 == 1) continue;
            if(ASMHooks.fixS[i] && j == 0 && index % 2 == 0) continue;
            if(ASMHooks.fixE[j] && i == 0 && index < 2) continue;
            if(ASMHooks.fixW[j] && i == 1 && index > 1) continue;

            if(flow[index] >= 8f/9) {
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
    public boolean isFluid(@Nonnull IBlockState up, @Nullable Fluid upFluid, @Nullable Fluid fluid, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing...faces) {
        if(isCompatibleFluid(fluid, upFluid)) {
            for(EnumFacing facing : faces) if(!canFluidFlow(world, pos, up, facing.getOpposite())) return false;
            return true;
        }

        return false;
    }

    //fixes crash when trying to render invalid state (temporary fix)
    public <V> IExtendedBlockState withPropertyFallback(@Nonnull IExtendedBlockState state, @Nonnull IUnlistedProperty<V> property, @Nullable V value, V fallback) {
        return state.withProperty(property, property.isValid(value) ? value : fallback);
    }

    public double getFlowDirection(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final Vec3d vec = getFlow(world, pos, state);
        return vec.x == 0 && vec.z == 0 ? -1000 : Math.atan2(vec.z, vec.x) - Math.PI / 2;
    }

    //FluidStates update properly
    @Override
    public void neighborChanged(@Nonnull IBlockState state, @Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull Block blockIn, @Nonnull BlockPos fromPos) {
        if(!FluidState.get(worldIn, pos).isEmpty()) worldIn.scheduleUpdate(pos, this, tickRate(worldIn));
    }

    @Override
    public boolean isReplaceable(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) { return true; }

    @Override
    public boolean requiresUpdates() { return false; }

    @Nonnull
    @Override
    public Fluid getFluid() { return (blockMaterial == Material.WATER) ? FluidRegistry.WATER : FluidRegistry.LAVA; }

    @Override
    public int place(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace) {
        if(fluidStack.amount < Fluid.BUCKET_VOLUME) return 0;
        if(doPlace) {
            final IBlockState here = world.getBlockState(pos);
            final Fluid fluid = getFluid();

            if(!isStateFluidloggable(here, fluid)) world.setBlockState(pos, getDefaultState());
            else setFluidState(world, pos, here, FluidState.of(fluid), true);
        }

        return Fluid.BUCKET_VOLUME;
    }

    @Nullable
    @Override
    public FluidStack drain(@Nonnull World world, @Nonnull BlockPos pos, boolean doDrain) {
        final IBlockState here = world.getBlockState(pos);
        final FluidState fluidState = getFluidState(world, pos, here);

        if(fluidState.isEmpty() || fluidState.getLevel() != 0) return null;
        if(doDrain) {
            if(fluidState.getState() == here) world.setBlockState(pos, Blocks.AIR.getDefaultState());
            else setFluidState(world, pos, here, FluidState.EMPTY, false);
        }

        return new FluidStack(fluidState.getFluid(), Fluid.BUCKET_VOLUME);
    }

    @Override
    public boolean canDrain(@Nonnull World world, @Nonnull BlockPos pos) {
        return getFluidOrReal(world, pos).getValue(BlockLiquid.LEVEL) == 0;
    }

    @Override
    public float getFilledPercentage(@Nonnull World world, @Nonnull BlockPos pos) {
        return 1 - (BlockLiquid.getLiquidHeightPercent(getFluidOrReal(world, pos).getValue(BlockLiquid.LEVEL)) - (1f / 9));
    }

    //most modded BlockLiquid instances involve blocks that typically shouldn't be fluidloggable fluids (like coral)
    @Override
    public boolean isFluidloggableFluid(@Nonnull IBlockState fluidState) {
        final Block block = fluidState.getBlock();
        return block == Blocks.FLOWING_LAVA || block == Blocks.LAVA || block == Blocks.FLOWING_WATER || block == Blocks.WATER;
    }
}
