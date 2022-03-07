package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import git.jbredwards.fluidlogged_api.asm.plugins.ASMHooks;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.BlockStairs;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.property.PropertyFloat;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.Map;

import static git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@Mixin(BlockFluidBase.class)
public abstract class BlockFluidBaseMixin extends Block
{
    @Shadow(remap = false)
    protected float quantaPerBlockFloat, quantaFraction;

    @Shadow(remap = false)
    protected int quantaPerBlock, density, densityDir, tickRate;

    @Shadow(remap = false)
    protected Map<Block, Boolean> displacements;

    @Final
    @Shadow(remap = false)
    protected Fluid definedFluid;

    public BlockFluidBaseMixin(@Nonnull Material material) { super(material); }

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
    @Overwrite
    public boolean shouldSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState neighbor = world.getBlockState(pos.offset(side));
        final boolean isCompatible = isCompatibleFluid(getFluidState(world, pos.offset(side), neighbor).getFluid(), getFluid());

        if(side == (densityDir < 0 ? EnumFacing.UP : EnumFacing.DOWN))
            return !isCompatible || !canFluidFlow(world, pos.offset(side), neighbor, side.getOpposite());

        else return !isCompatible && super.shouldSideBeRendered(state, world, pos, side);
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Nonnull
    @Overwrite(remap = false)
    public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if (!(oldState instanceof IExtendedBlockState)) return oldState;

        //covert to extended state
        IExtendedBlockState state = (IExtendedBlockState) oldState;
        state = state.withProperty(BlockFluidBase.FLOW_DIRECTION, (float) BlockFluidBase.getFlowDirection(world, pos));

        //corner height variables
        final IBlockState[][] upBlockState = new IBlockState[3][3];
        final Fluid[][] upFluid = new Fluid[3][3];
        final float[][] height = new float[3][3];
        final float[][] corner = new float[2][2];

        upBlockState[1][1] = world.getBlockState(pos.down(densityDir));
        upFluid[1][1] = getFluidState(world, pos.down(densityDir), upBlockState[1][1]).getFluid();
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
                        upBlockState[i][j] = world.getBlockState(pos.add(i - 1, 0, j - 1).down(densityDir));
                        upFluid[i][j] = getFluidState(world, pos.add(i - 1, 0, j - 1).down(densityDir), upBlockState[i][j]).getFluid();
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
            final IBlockState here = world.getBlockState(pos);
            if(fixCorner(here, world, pos, EnumFacing.NORTH, EnumFacing.WEST) || fixCorner(here, world, pos, EnumFacing.WEST, EnumFacing.NORTH)) corner[0][0] = quantaFraction;
            if(fixCorner(here, world, pos, EnumFacing.SOUTH, EnumFacing.WEST) || fixCorner(here, world, pos, EnumFacing.WEST, EnumFacing.SOUTH)) corner[0][1] = quantaFraction;
            if(fixCorner(here, world, pos, EnumFacing.NORTH, EnumFacing.EAST) || fixCorner(here, world, pos, EnumFacing.EAST, EnumFacing.NORTH)) corner[1][0] = quantaFraction;
            if(fixCorner(here, world, pos, EnumFacing.SOUTH, EnumFacing.EAST) || fixCorner(here, world, pos, EnumFacing.EAST, EnumFacing.SOUTH)) corner[1][1] = quantaFraction;
        }

        //side overlays
        for(int i = 0; i < 4; i++) {
            EnumFacing side = EnumFacing.getHorizontal(i);
            BlockPos offset = pos.offset(side);
            boolean useOverlay = world.getBlockState(offset).getBlockFaceShape(world, offset, side.getOpposite()) == BlockFaceShape.SOLID;
            state = state.withProperty(BlockFluidBase.SIDE_OVERLAYS[i], useOverlay);
        }
        //sets the corner props
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[0], corner[0][0]);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[1], corner[0][1]);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[2], corner[1][1]);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[3], corner[1][0]);
        return state;
    }

    private boolean fixCorner(@Nonnull IBlockState here, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing primary, @Nonnull EnumFacing other) {
        if(canFluidFlow(world, pos, here, primary)) return false;

        final BlockPos offset = pos.offset(other);
        final IBlockState neighbor = world.getBlockState(offset);

        if(!canFluidFlow(world, offset, neighbor, primary) || !canFluidFlow(world, offset, neighbor, other.getOpposite()))
            return true;

        else return !isCompatibleFluid(getFluidState(world, offset, neighbor).getFluid(), getFluid());
    }

    private float getFluidHeightForRender(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState up, @Nullable Fluid upFluid, int i, int j) {
        //check block above
        if(isFluid(up, upFluid, world, pos.down(densityDir), (densityDir < 0 ? EnumFacing.UP : EnumFacing.DOWN))) return 1;

        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        final boolean canSideFlow = ASMHooks.canSideFlow(getFluid(), state, world, pos, i, j);
        final boolean fluidMatches = isCompatibleFluid(fluidState.getFluid(), getFluid());

        //is a fluid
        if(fluidMatches && canSideFlow) {
            final int level = fluidState.getLevel();

            if(level == getMaxRenderHeightMeta()) return quantaFraction;
            else return ((quantaPerBlock - level) / quantaPerBlockFloat) * quantaFraction;
        }

        //not a fluid
        else return -1f / quantaPerBlock * quantaFraction;
    }

    private float getFluidHeightAverage(int i, int j, @Nonnull float... flow) {
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

    private boolean isFluid(@Nonnull IBlockState neighbor, @Nullable Fluid fluid, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing...faces) {
        if(isCompatibleFluid(getFluid(), fluid)) {
            for(EnumFacing facing : faces) if(!canFluidFlow(world, pos, neighbor, facing.getOpposite())) return false;
            return true;
        }

        return false;
    }

    private IExtendedBlockState withPropertyFallback(@Nonnull IExtendedBlockState state, @Nonnull PropertyFloat property, float value) {
        return state.withProperty(property, property.isValid(value) ? value : quantaFraction);
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
                int otherDecay = getFlowDecayOffset(world, offset, facing.getOpposite());

                if(otherDecay >= quantaPerBlock) {
                    otherDecay = getFlowDecay(world, offset.up(densityDir));

                    if(otherDecay < quantaPerBlock) {
                        int power = otherDecay - (decay - quantaPerBlock);
                        vec = vec.addVector(facing.getFrontOffsetX() * power, 0, facing.getFrontOffsetZ() * power);
                    }
                }
                else {
                    int power = otherDecay - decay;
                    vec = vec.addVector(facing.getFrontOffsetX() * power, 0, facing.getFrontOffsetZ() * power);
                }
            }
        }

        if(hasVerticalFlow(world, pos)) {
            for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                BlockPos offset = pos.offset(facing);
                if(causesDownwardCurrent(world, offset, facing) || causesDownwardCurrent(world, offset.down(densityDir), facing)) {
                    vec = vec.normalize().addVector(0.0, 6.0 * densityDir, 0.0);
                    break;
                }
            }
        }

        return vec.normalize();
    }

    private int getFlowDecayOffset(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing facing) {
        if(!canFluidFlow(world, pos, world.getBlockState(pos), facing)) return quantaPerBlock + 1;
        else return getFlowDecay(world, pos);
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
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    protected boolean causesDownwardCurrent(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState state = worldIn.getBlockState(pos);
        if(canFluidFlow(worldIn, pos, state, side) && isCompatibleFluid(getFluidState(worldIn, pos, state).getFluid(), getFluid()))
            return false;

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
    @Redirect(method = "getFlowDirection", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"), remap = false)
    private static IBlockState getBlockState(@Nonnull IBlockAccess world, @Nonnull BlockPos upPos) {
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
    public abstract int getMaxRenderHeightMeta();

    @Shadow(remap = false)
    protected abstract int getFlowDecay(@Nonnull IBlockAccess world, @Nonnull BlockPos pos);
}
