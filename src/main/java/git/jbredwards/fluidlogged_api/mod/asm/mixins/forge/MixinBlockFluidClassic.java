package git.jbredwards.fluidlogged_api.mod.asm.mixins.forge;

import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.BlockFluidClassic;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.List;
import java.util.Random;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@Mixin(BlockFluidClassic.class)
public abstract class MixinBlockFluidClassic extends MixinBlockFluidBase implements IFluidloggableFluid
{
    @Final
    @Shadow(remap = false)
    protected static List<EnumFacing> SIDES;

    @Shadow(remap = false)
    protected boolean[] isOptimalFlowDirection;

    @Shadow(remap = false)
    protected int[] flowCost;

    @Shadow(remap = false)
    protected boolean canCreateSources;

    @Shadow(remap = false)
    protected FluidStack stack;

    public MixinBlockFluidClassic(@Nonnull Material materialIn) { super(materialIn); }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    public int getQuantaValue(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        if(!isCompatibleFluid(fluidState.getFluid(), getFluid())) return -1;
        else return quantaPerBlock - fluidState.getLevel();
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite
    public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
        if(!world.isAreaLoaded(pos, quantaPerBlock / 2)) return; // Forge: avoid loading unloaded chunks

        final IBlockState here = world.getBlockState(pos); //fluidlogged fluids will have a different state here than the state input
        final EnumFacing facingDir = (densityDir > 0) ? EnumFacing.UP : EnumFacing.DOWN;
        int quantaRemaining = quantaPerBlock - state.getValue(BlockLiquid.LEVEL);

        // check adjacent block levels if non-source
        if(quantaRemaining < quantaPerBlock) {
            int adjacentSourceBlocks = 0;
            final int expQuanta;

            if(ForgeEventFactory.canCreateFluidSource(world, pos, state, canCreateSources)) {
                for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                    BlockPos offset = pos.offset(facing);

                    if(isSourceBlock(world, offset, world.getBlockState(offset), facing.getOpposite()))
                        adjacentSourceBlocks++;
                }
            }

            // new source block
            final IBlockState vertical = world.getBlockState(pos.up(densityDir));
            if(adjacentSourceBlocks >= 2 && (vertical.getMaterial().isSolid() || isSourceBlock(world, pos.up(densityDir), vertical, facingDir.getOpposite())))
                expQuanta = quantaPerBlock;

            // vertical flow into block
            else if(hasVerticalFlow(world, pos)) expQuanta = quantaPerBlock - 1;

            else {
                int maxQuanta = -100;
                for(EnumFacing side : EnumFacing.HORIZONTALS) {
                    BlockPos offset = pos.offset(side);

                    if(canFluidFlow(world, pos, here, side) && canFluidFlow(world, offset, world.getBlockState(offset), side.getOpposite()))
                        maxQuanta = getLargerQuanta(world, offset, maxQuanta);
                }

                expQuanta = maxQuanta - 1;
            }

            // decay calculation
            if(expQuanta != quantaRemaining) {
                quantaRemaining = expQuanta;

                if(expQuanta <= 0) world.setBlockToAir(pos);
                else {
                    world.setBlockState(pos, state.withProperty(BlockLiquid.LEVEL, quantaPerBlock - expQuanta), Constants.BlockFlags.SEND_TO_CLIENTS);
                    world.scheduleUpdate(pos, this, tickRate);
                    world.notifyNeighborsOfStateChange(pos, this, false);
                }
            }
        }

        //try flowing to nearby fluidloggable blocks
        else tryFlowIntoFluidloggable(world, pos, facingDir, state, here, EnumFacing.HORIZONTALS);

        // Flow vertically if possible
        tryFlowIntoFluidloggable(world, pos, facingDir, state, here, facingDir);
        if(canFluidFlow(world, pos, here, facingDir) && canDisplace(world, pos.up(densityDir))) {
            flowIntoBlock(world, pos.up(densityDir), 1);
            return;
        }

        // Flow outward if possible
        int flowMeta = quantaPerBlock - quantaRemaining + 1;
        if(flowMeta >= quantaPerBlock) return;

        if(isSourceBlock(world, pos, here, null) || !isFlowingVertically(world, pos)) {
            if(hasVerticalFlow(world, pos)) flowMeta = 1;

            final boolean[] flowTo = getOptimalFlowDirections(world, pos, here);
            for(int i = 0; i < 4; i++)
                if(flowTo[i] && canFluidFlow(world, pos, here, SIDES.get(i)))
                    flowIntoBlock(world, pos.offset(SIDES.get(i)), flowMeta);
        }
    }

    //try flowing to nearby fluidloggable blocks
    private void tryFlowIntoFluidloggable(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumFacing facingDir, @Nonnull IBlockState state, @Nonnull IBlockState here, @Nonnull EnumFacing... flowInto) {
        if(quantaPerBlock > 0 && ConfigHandler.fluidloggedFluidSpread > 0 && canCreateSources && (ConfigHandler.fluidloggedFluidSpread == 2 || state != here) && (state != here || isFluidloggableFluid(state, world, pos))) {
            for(EnumFacing facing : flowInto) {
                if(canFluidFlow(world, pos, here, facing)) {
                    BlockPos offset = pos.offset(facing);
                    IBlockState neighbor = world.getBlockState(offset);

                    //check if the fluid could occupy the space
                    if(canFluidFlow(world, offset, neighbor, facing.getOpposite()) && isStateFluidloggable(neighbor, world, offset, getFluid()) && FluidState.get(world, offset).isEmpty()) {
                        //check for another source block that can flow into this
                        for(EnumFacing adjacentFacing : EnumFacing.values()) {
                            if(adjacentFacing != facingDir && adjacentFacing != facing.getOpposite() && canFluidFlow(world, offset, neighbor, adjacentFacing)) {
                                BlockPos adjacentOffset = offset.offset(adjacentFacing);
                                IBlockState adjacent = world.getBlockState(adjacentOffset);

                                if(canFluidFlow(world, adjacentOffset, adjacent, adjacentFacing.getOpposite())) {
                                    //only allow certain FluidStates to count
                                    FluidState adjacentFluid = ConfigHandler.fluidloggedFluidSpread == 1
                                            ? FluidState.get(world, adjacentOffset)
                                            : getFluidState(world, adjacentOffset, adjacent);

                                    //set the FluidState in the world
                                    if(isCompatibleFluid(adjacentFluid.getFluid(), getFluid()) && FluidloggedUtils.isFluidloggableFluid(adjacentFluid.getState(), world, adjacentOffset)) {
                                        setFluidState(world, offset, neighbor, FluidState.of(this), false);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    public boolean isFlowingVertically(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final EnumFacing facingDir = (densityDir < 0) ? EnumFacing.UP : EnumFacing.DOWN;

        final IBlockState here = world.getBlockState(pos);
        if(!canFluidFlow(world, pos, here, facingDir.getOpposite())) return false;

        final IBlockState neighbor = world.getBlockState(pos.up(densityDir));
        return isCompatibleFluid(getFluidState(world, pos.up(densityDir), neighbor).getFluid(), getFluid())
                || (isCompatibleFluid(getFluidState(world, pos, here).getFluid(), getFluid())
                && canFlowInto(world, pos.up(densityDir)));
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    public boolean isSourceBlock(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return isSourceBlock(world, pos, world.getBlockState(pos), null);
    }

    private boolean isSourceBlock(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nullable EnumFacing facing) {
        if(facing != null && !canFluidFlow(world, pos, here, facing)) return false;

        final FluidState fluidState = getFluidState(world, pos, here);
        return isCompatibleFluid(fluidState.getFluid(), getFluid()) && fluidState.getLevel() == 0;
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    protected boolean[] getOptimalFlowDirections(@Nonnull World world, @Nonnull BlockPos pos) {
        return getOptimalFlowDirections(world, pos, world.getBlockState(pos));
    }

    private boolean[] getOptimalFlowDirections(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
        for(int side = 0; side < 4; side++) {
            flowCost[side] = 1000;
            if(!canFluidFlow(world, pos, here, SIDES.get(side))) continue;

            BlockPos offset = pos.offset(SIDES.get(side));
            if(!canFlowInto(world, offset) || isSourceBlock(world, offset)) continue;

            if(canFlowInto(world, offset.up(densityDir))) flowCost[side] = 0;
            else flowCost[side] = calculateFlowCost(world, offset, 1, side);
        }

        int min = Ints.min(flowCost);
        for(int side = 0; side < 4; side++) isOptimalFlowDirection[side] = (flowCost[side] == min);

        return isOptimalFlowDirection;
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    protected boolean canFlowInto(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return isCompatibleFluid(getFluidState(world, pos).getFluid(), getFluid()) || canDisplace(world, pos);
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    public int place(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace) {
        if(fluidStack.amount < Fluid.BUCKET_VOLUME) return 0;
        if(doPlace) {
            final IBlockState here = world.getBlockState(pos);
            final Fluid fluid = getFluid();

            if(isStateFluidloggable(here, world, pos, fluid)) setFluidState(world, pos, here, FluidState.of(fluid), true);
            else world.setBlockState(pos, getDefaultState(), Constants.BlockFlags.DEFAULT_AND_RERENDER);
        }

        return Fluid.BUCKET_VOLUME;
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Nullable
    @Overwrite(remap = false)
    public FluidStack drain(@Nonnull World world, @Nonnull BlockPos pos, boolean doDrain) {
        final IBlockState here = world.getBlockState(pos);
        final FluidState fluidState = getFluidState(world, pos, here);

        if(fluidState.isEmpty()) return null;
        if(doDrain) {
            if(fluidState.getState() == here) world.setBlockState(pos, Blocks.AIR.getDefaultState());
            else setFluidState(world, pos, here, FluidState.EMPTY, false);
        }

        return fluidState.getLevel() != 0 ? null : stack.copy();
    }

    @Override
    public boolean isFluidloggableFluid(@Nonnull IBlockState fluid, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(!isFluidloggableFluid()) return false;
        else if(fluid.getValue(BlockLiquid.LEVEL) == getMaxRenderHeightMeta()) return true;
        else if(!canCreateSources) return false;

        final IBlockState vertical = world.getBlockState(pos.down(densityDir));
        return isCompatibleFluid(getFluid(), getFluidState(world, pos.down(densityDir), vertical).getFluid())
                && canFluidFlow(world, pos.down(densityDir), vertical, densityDir < 1 ? EnumFacing.DOWN : EnumFacing.UP);
    }

    @Override
    public boolean isFluidloggableFluid() { return !hasTileEntity() && this == getFluid().getBlock(); }

    @Shadow(remap = false)
    public abstract int getMaxRenderHeightMeta();

    @Shadow(remap = false)
    protected abstract int getLargerQuanta(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare);

    @Shadow(remap = false)
    protected abstract int calculateFlowCost(@Nonnull World world, @Nonnull BlockPos pos, int recurseDepth, int side);

    @Shadow(remap = false)
    protected abstract void flowIntoBlock(@Nonnull World world, @Nonnull BlockPos pos, int meta);
}
