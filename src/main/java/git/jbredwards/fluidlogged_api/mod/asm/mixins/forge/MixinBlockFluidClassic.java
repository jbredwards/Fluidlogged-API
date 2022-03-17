package git.jbredwards.fluidlogged_api.mod.asm.mixins.forge;

import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import git.jbredwards.fluidlogged_api.mod.common.legacy.LegacyWorldFixer;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.MapColor;
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
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

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
@SuppressWarnings("unused")
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
        if(!isCompatibleFluid(world, fluidState.getFluid(), getFluid())) return -1;
        else return quantaPerBlock - fluidState.getLevel();
    }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
        final IBlockState here = world.getBlockState(pos); //fluidlogged fluids will have a different state here than the state input
        final EnumFacing facingDir = (densityDir < 0) ? EnumFacing.UP : EnumFacing.DOWN;
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
            if(adjacentSourceBlocks >= 2 && (vertical.getMaterial().isSolid() || isSourceBlock(world, pos.up(densityDir), vertical, facingDir)))
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
        else if(quantaPerBlock > 0 && ConfigHandler.fluidloggedFluidSpread > 0 && canCreateSources && (ConfigHandler.fluidloggedFluidSpread == 2 || state != here) && (state != here || isFluidloggableFluid(state, false))) {
            for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                if(canFluidFlow(world, pos, here, facing)) {
                    BlockPos offset = pos.offset(facing);
                    IBlockState neighbor = world.getBlockState(offset);

                    //check if the fluid could occupy the space
                    if(canFluidFlow(world, offset, neighbor, facing.getOpposite()) && isStateFluidloggable(neighbor, getFluid()) && FluidState.get(world, offset).isEmpty()) {
                        //check for another source block that can flow into this
                        for(EnumFacing adjacentFacing : EnumFacing.HORIZONTALS) {
                            if(adjacentFacing != facing.getOpposite() && canFluidFlow(world, offset, neighbor, adjacentFacing)) {
                                BlockPos adjacentOffset = offset.offset(adjacentFacing);
                                IBlockState adjacent = world.getBlockState(adjacentOffset);

                                if(canFluidFlow(world, adjacentOffset, adjacent, adjacentFacing.getOpposite())) {
                                    //only allow certain FluidStates to count
                                    FluidState adjacentFluid = ConfigHandler.fluidloggedFluidSpread == 1
                                            ? FluidState.get(world, adjacentOffset)
                                            : getFluidState(world, adjacentOffset, adjacent);

                                    //set the FluidState in the world
                                    if(isCompatibleFluid(world, adjacentFluid.getFluid(), getFluid())) {
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

        // Flow vertically if possible
        if(canFluidFlow(world, pos, here, facingDir.getOpposite()) && canDisplace(world, pos.up(densityDir))) {
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
        return isCompatibleFluid(world, getFluidState(world, pos.up(densityDir), neighbor).getFluid(), getFluid())
                || (isCompatibleFluid(world, getFluidState(world, pos, here).getFluid(), getFluid())
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
        return isCompatibleFluid(world, fluidState.getFluid(), getFluid()) && fluidState.getLevel() == 0;
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
        return isCompatibleFluid(world, getFluidState(world, pos).getFluid(), getFluid()) || canDisplace(world, pos);
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

            if(isStateFluidloggable(here, fluid)) setFluidState(world, pos, here, FluidState.of(fluid), true);
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

        if(fluidState.isEmpty() || fluidState.getLevel() != 0) return null;
        if(doDrain) {
            if(fluidState.getState() == here) world.setBlockState(pos, Blocks.AIR.getDefaultState());
            else setFluidState(world, pos, here, FluidState.EMPTY, false);
        }

        return stack.copy();
    }

    @Override
    public boolean isFluidloggableFluid(@Nonnull IBlockState fluid, boolean checkLevel) {
        if(hasTileEntity(fluid) || this != getFluid().getBlock()) return false;
        else if(!checkLevel) return true;

        final int level = fluid.getValue(BlockLiquid.LEVEL);
        return level == 0 || level >= quantaPerBlock && canCreateSources;
    }

    @Shadow(remap = false)
    protected abstract int getLargerQuanta(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare);

    @Shadow(remap = false)
    protected abstract int calculateFlowCost(@Nonnull World world, @Nonnull BlockPos pos, int recurseDepth, int side);

    @Shadow(remap = false)
    protected abstract void flowIntoBlock(@Nonnull World world, @Nonnull BlockPos pos, int meta);

    /**
     * help legacy worlds with modded fluidlogged fluids work
     */
    @Inject(method = "<init>(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/material/Material;Lnet/minecraft/block/material/MapColor;)V", at = @At("RETURN"), remap = false)
    private void registerLegacyWorldFixes(@Nonnull Fluid fluid, @Nonnull Material material, @Nonnull MapColor mapColor, @Nonnull CallbackInfo ci) {
        if(ConfigHandler.enableLegacyCompat && fluid.getBlock() == this && FluidRegistry.isFluidRegistered(fluid)) {
            ForgeRegistries.BLOCKS.register(new LegacyWorldFixer(fluid));
        }
    }
}
