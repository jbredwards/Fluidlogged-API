package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.ASMHooks;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IExtendedBlockState;
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
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

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

        return flag ? 1 : Math.max(1 - (BlockLiquid.getLiquidHeightPercent(state.getValue(BlockLiquid.LEVEL)) - 1f/9), 0);
    }

    @Nonnull
    @Redirect(method = "getFogColor", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFogColor(@Nonnull World world, @Nonnull BlockPos upPos) { return getFluidOrReal(world, upPos); }

    @Nonnull
    @Override
    public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(!(oldState instanceof IExtendedBlockState)) return oldState;
        final IBlockState here = world.getBlockState(pos);

        //covert to extended state
        IExtendedBlockState state = (IExtendedBlockState)oldState;
        state = state.withProperty(BlockFluidBase.FLOW_DIRECTION, getSlopeAngle(world, pos));

        //corner height variables
        final IBlockState[][] upBlockState = new IBlockState[3][3];
        final Fluid[][] upFluid = new Fluid[3][3];
        final float[][] height = new float[3][3];
        final float[][] corner = new float[2][2];

        upBlockState[1][1] = world.getBlockState(pos.up());
        upFluid[1][1] = getFluidState(world, pos.up(), upBlockState[1][1]).getFluid();
        height[1][1] = getFluidHeightForRender(world, pos, upBlockState[1][1], upFluid[1][1], 1, 1);

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
                        height[i][j] = getFluidHeightForRender(world, pos.add(i - 1, 0, j - 1), upBlockState[i][j], upFluid[i][j], i, j);
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
            boolean n =  isFluid(upBlockState[0][1], upFluid[0][1], world, pos.north(), EnumFacing.NORTH);
            boolean s =  isFluid(upBlockState[2][1], upFluid[2][1], world, pos.south(), EnumFacing.SOUTH);
            boolean w =  isFluid(upBlockState[1][0], upFluid[1][0], world, pos.west(),  EnumFacing.WEST);
            boolean e =  isFluid(upBlockState[1][2], upFluid[1][2], world, pos.east(),  EnumFacing.EAST);
            boolean nw = isFluid(upBlockState[0][0], upFluid[0][0], world, pos.north().west(), EnumFacing.NORTH, EnumFacing.WEST);
            boolean ne = isFluid(upBlockState[0][2], upFluid[0][2], world, pos.north().east(), EnumFacing.NORTH, EnumFacing.EAST);
            boolean sw = isFluid(upBlockState[2][0], upFluid[2][0], world, pos.south().west(), EnumFacing.SOUTH, EnumFacing.WEST);
            boolean se = isFluid(upBlockState[2][2], upFluid[2][2], world, pos.south().east(), EnumFacing.SOUTH, EnumFacing.EAST);
            if(nw || n || w) corner[0][0] = 1;
            if(ne || n || e) corner[0][1] = 1;
            if(sw || s || w) corner[1][0] = 1;
            if(se || s || e) corner[1][1] = 1;

            //fix corners of fluidlogged blocks
            if(corner[0][0] < 8f/9 && (fixCorner(here, world, pos, EnumFacing.NORTH, EnumFacing.WEST) || fixCorner(here, world, pos, EnumFacing.WEST, EnumFacing.NORTH))) corner[0][0] = 8f/9;
            if(corner[0][1] < 8f/9 && (fixCorner(here, world, pos, EnumFacing.SOUTH, EnumFacing.WEST) || fixCorner(here, world, pos, EnumFacing.WEST, EnumFacing.SOUTH))) corner[0][1] = 8f/9;
            if(corner[1][0] < 8f/9 && (fixCorner(here, world, pos, EnumFacing.NORTH, EnumFacing.EAST) || fixCorner(here, world, pos, EnumFacing.EAST, EnumFacing.NORTH))) corner[1][0] = 8f/9;
            if(corner[1][1] < 8f/9 && (fixCorner(here, world, pos, EnumFacing.SOUTH, EnumFacing.EAST) || fixCorner(here, world, pos, EnumFacing.EAST, EnumFacing.SOUTH))) corner[1][1] = 8f/9;
        }

        //side overlays
        for(int i = 0; i < 4; i++) {
            EnumFacing side = EnumFacing.byHorizontalIndex(i);
            BlockPos offset = pos.offset(side);
            boolean useOverlay = world.getBlockState(offset).getBlockFaceShape(world, offset, side.getOpposite()) == BlockFaceShape.SOLID;
            state = state.withProperty(BlockFluidBase.SIDE_OVERLAYS[i], useOverlay);
        }

        //fix possible top z fighting
        if(!canFluidFlow(world, pos, here, EnumFacing.UP)) {
            if(corner[0][0] == 1) corner[0][0] = 0.998f;
            if(corner[0][1] == 1) corner[0][1] = 0.998f;
            if(corner[1][0] == 1) corner[1][0] = 0.998f;
            if(corner[1][1] == 1) corner[1][1] = 0.998f;
        }

        //sets the corner props
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[0], corner[0][0], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[1], corner[0][1], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[2], corner[1][1], 8f/9);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[3], corner[1][0], 8f/9);
        return state;
    }

    //fixes issue#59
    private float getSlopeAngle(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final Vec3d vec = getFlow(world, pos, world.getBlockState(pos));
        return vec.x == 0 && vec.z == 0 ? -1000 : (float)MathHelper.atan2(vec.z, vec.x) - (float)(Math.PI / 2);
    }

    //used by getExtendedState
    private boolean fixCorner(@Nonnull IBlockState here, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing primary, @Nonnull EnumFacing other) {
        if(canFluidFlow(world, pos, here, primary)) return false;

        final BlockPos offset = pos.offset(other);
        final IBlockState neighbor = world.getBlockState(offset);

        if(!canFluidFlow(world, offset, neighbor, primary) || !canFluidFlow(world, offset, neighbor, other.getOpposite()))
            return true;

        else return !isCompatibleFluid(getFluidState(world, offset, neighbor).getFluid(), getFluid());
    }

    //used by getExtendedState
    private float getFluidHeightForRender(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState up, @Nullable Fluid upFluid, int i, int j) {
        //check block above
        if(isFluid(up, upFluid, world, pos.up(), EnumFacing.UP)) return 1;
        final IBlockState state = world.getBlockState(pos);

        //is air
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        final boolean canSideFlow = ASMHooks.canSideFlow(state, world, pos, i, j);
        final boolean fluidMatches = isCompatibleFluid(fluidState.getFluid(), getFluid());

        //is a fluid
        if(fluidMatches && canSideFlow) {
            final int level = fluidState.getLevel();

            if(level == 0) return 8f / 9;
            else return ((8 - level) / 8f) * (8f / 9);
        }

        //not a fluid
        else return (-1 / 8f) * (8f / 9);
    }

    //used by getExtendedState
    private float getFluidHeightAverage(int i, int j, @Nonnull float... flow) {
        float total = 0;
        int count = 0;

        for(int index = 0; index < flow.length; index++) {
            //fix corners visually flowing into illegal sides (vanilla 1.13 bug)
            if(ASMHooks.fixN[i] && j == 1 && (index & 1) == 1) continue;
            if(ASMHooks.fixS[i] && j == 0 && (index & 1) == 0) continue;
            if(ASMHooks.fixE[j] && i == 0 && index <= 1) continue;
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
    private boolean isFluid(@Nonnull IBlockState up, @Nullable Fluid upFluid, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing...faces) {
        if(isCompatibleFluid(getFluid(), upFluid)) {
            for(EnumFacing facing : faces) if(!canFluidFlow(world, pos, up, facing.getOpposite())) return false;
            return true;
        }

        return false;
    }

    //fixes crash when trying to render invalid state (temporary fix)
    private <V> IExtendedBlockState withPropertyFallback(@Nonnull IExtendedBlockState state, @Nonnull IUnlistedProperty<V> property, @Nullable V value, V fallback) {
        return state.withProperty(property, property.isValid(value) ? value : fallback);
    }

    @Nonnull
    @Override
    public Fluid getFluid() { return (material == Material.WATER) ? FluidRegistry.WATER : FluidRegistry.LAVA; }

    @Override
    public int place(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace) {
        if(fluidStack.amount < Fluid.BUCKET_VOLUME) return 0;
        if(doPlace) {
            final IBlockState here = world.getBlockState(pos);

            if(isStateFluidloggable(here, world, pos, getFluid())) setFluidState(world, pos, here, FluidState.of(this), true);
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
            else setFluidState(world, pos, here, FluidState.EMPTY, false);
        }

        return fluidState.getLevel() != 0 ? null : new FluidStack(fluidState.getFluid(), Fluid.BUCKET_VOLUME);
    }

    @Override
    public boolean canDrain(@Nonnull World world, @Nonnull BlockPos pos) { return getFluidState(world, pos).getLevel() == 0; }

    @Override
    public float getFilledPercentage(@Nonnull World world, @Nonnull BlockPos pos) { return getFilledPercentage((IBlockAccess)world, pos); }
    public float getFilledPercentage(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return hasVerticalFlow(world, pos) ? 1 : 1 - (BlockLiquid.getLiquidHeightPercent(getFluidState(world, pos).getLevel()) - (1f/9));
    }

    @Override
    public boolean isFluidloggableFluid(@Nonnull IBlockState fluid, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
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
        return materialIn == material && entity.getEntityBoundingBox().minY < pos.getY() + getFilledPercentage(world, pos);
    }

    @Nullable
    @Override
    public Boolean isAABBInsideMaterial(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB boundingBox, @Nonnull Material materialIn) {
        return materialIn == material && Boolean.TRUE.equals(isAABBInsideLiquid(world, pos, boundingBox));
    }

    @Nullable
    @Override
    public Boolean isAABBInsideLiquid(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB boundingBox) {
        return boundingBox.minY < pos.getY() + getFilledPercentage(world, pos);
    }
}
