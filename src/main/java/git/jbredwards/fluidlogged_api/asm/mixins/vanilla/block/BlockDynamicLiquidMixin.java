package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.BlockDynamicLiquid;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.Fluid;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import static git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils.*;

/**
 * -vanilla fluids no longer do block mixing when they shouldn't
 * -vanilla fluids now flow from fluidlogged blocks
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockDynamicLiquid.class)
public abstract class BlockDynamicLiquidMixin extends BlockLiquidMixin
{
    @Shadow
    int adjacentSourceBlocks;

    protected BlockDynamicLiquidMixin(@Nonnull Material materialIn) { super(materialIn); }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
        if(!world.isAreaLoaded(pos, getSlopeFindDistance(world))) return; //prevent loading unnecessary chunks

        final Fluid fluid = getFluid();
        final IBlockState here = world.getBlockState(pos); //the state here can be different to the state input
        final IBlockState down = world.getBlockState(pos.down());
        final FluidState downFluidState = getFluidState(world, pos.down(), down);

        final int lavaDif = (blockMaterial == Material.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
        int stateLevel = state.getValue(BlockLiquid.LEVEL);
        int tickRate = tickRate(world);

        //non source block & non FluidState
        if(stateLevel > 0) {
            int currentMinLevel = -100;
            adjacentSourceBlocks = 0;

            //used in place of instance#checkAdjacentBlock
            for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                if(canFluidFlow(world, pos, here, facing)) {
                    BlockPos offset = pos.offset(facing);
                    IBlockState neighbor = world.getBlockState(offset);
                    FluidState fluidNeighbor = getFluidState(world, offset, neighbor);

                    if(isCompatibleFluid(fluidNeighbor.getFluid(), fluid) && canFluidFlow(world, offset, neighbor, facing.getOpposite())) {
                        int neighborLevel = fluidNeighbor.getLevel();

                        if(neighborLevel == 0) adjacentSourceBlocks++;
                        else if(neighborLevel >= 8) neighborLevel = 0;

                        if(currentMinLevel < 0 || neighborLevel < currentMinLevel)
                            currentMinLevel = neighborLevel;
                    }
                }
            }

            int futureLevel = currentMinLevel + lavaDif;
            if(futureLevel >= 8 || currentMinLevel < 0) futureLevel = -1;

            //used in place of instance#getDepth
            final IBlockState up = world.getBlockState(pos.up());
            final FluidState fluidStateUp = getFluidState(world, pos.up(), up);
            final int depth = (isCompatibleFluid(fluidStateUp.getFluid(), fluid)
                    && canFluidFlow(world, pos.up(), up, EnumFacing.DOWN)) ? fluidStateUp.getLevel() : -1;

            if(depth >= 0) futureLevel = (depth >= 8) ? depth : depth + 8;

            //prepare source block creation
            if(adjacentSourceBlocks >= 2 && ForgeEventFactory.canCreateFluidSource(world, pos, state, state.getMaterial() == Material.WATER)) {
                if(down.getMaterial().isSolid())
                    futureLevel = 0;

                else if(isCompatibleFluid(downFluidState.getFluid(), fluid) && downFluidState.getLevel() == 0)
                    futureLevel = 0;
            }

            //varies lava flow speed slightly
            if(state.getMaterial() == Material.LAVA && futureLevel < 8 && futureLevel > stateLevel && rand.nextInt(4) != 0)
                tickRate *= 4;

            //reduce lag by placing static block if there's no change
            if(futureLevel == stateLevel) placeStaticBlock(world, pos, state);

            //still a flowing block
            else {
                stateLevel = futureLevel;

                //update state that exists in the world
                if(stateLevel < 0) world.setBlockState(pos, Blocks.AIR.getDefaultState());
                else {
                    state = state.withProperty(BlockLiquid.LEVEL, stateLevel);
                    world.setBlockState(pos, state, Constants.BlockFlags.SEND_TO_CLIENTS);
                    world.scheduleUpdate(pos, this, tickRate);
                    world.notifyNeighborsOfStateChange(pos, this, false);
                }
            }
        }

        //set to static block if non-FluidState
        else if(state == here) placeStaticBlock(world, pos, state);

        //flow down
        if(canFlowInto(world, pos.down(), down)) {
            if(state.getMaterial() == Material.LAVA) {
                if(!downFluidState.isEmpty() && downFluidState.getMaterial() == Material.WATER && down.getBlock().isReplaceable(world, pos.down())) {
                    world.setBlockState(pos.down(), ForgeEventFactory.fireFluidPlaceBlockEvent(world, pos.down(), pos, Blocks.STONE.getDefaultState()));
                    triggerMixEffects(world, pos.down());
                    return;
                }
            }

            tryFlowInto(world, pos.down(), down, (stateLevel >= 8) ? stateLevel : stateLevel + 8);
        }

        //try flowing to the side
        else if(stateLevel >= 0 && (stateLevel == 0 || isBlocked(world, pos.down(), down))) {
            final int flowingLevel = (stateLevel >= 8) ? 1 : stateLevel + lavaDif;
            if(flowingLevel >= 8) return;

            final Set<EnumFacing> flowDirections = getPossibleFlowDirections(world, pos);
            for(EnumFacing facing : flowDirections) {
                if(canFluidFlow(world, pos, here, facing))
                    tryFlowInto(world, pos.offset(facing), world.getBlockState(pos.offset(facing)), flowingLevel);
            }
        }
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    private void tryFlowInto(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state, int level) {
        if(state.getMaterial() != Material.AIR) {
            if(blockMaterial == Material.LAVA) triggerMixEffects(worldIn, pos);
            else if(state.getBlock() != Blocks.SNOW_LAYER) //Forge: Vanilla has a 'bug' where snowballs don't drop like every other block. So special case because ewww...
            state.getBlock().dropBlockAsItem(worldIn, pos, state, 0);
        }

        worldIn.setBlockState(pos, getDefaultState().withProperty(BlockLiquid.LEVEL, level));
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    private int getSlopeDistance(@Nonnull World world, @Nonnull BlockPos pos, int distance, @Nonnull EnumFacing calculateFlowCost) {
        int shortestDistance = 1000;
        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            if(facing != calculateFlowCost) {
                BlockPos offset = pos.offset(facing);
                IBlockState neighbor = world.getBlockState(offset);
                FluidState fluidNeighbor = getFluidState(world, offset, neighbor);

                if(!isBlocked(world, offset, neighbor) && (!isCompatibleFluid(fluidNeighbor.getFluid(), getFluid()) || fluidNeighbor.getLevel() > 0)) {
                    if(!isBlocked(world, offset.down(), world.getBlockState(offset.down()))) return distance;

                    else if(distance < getSlopeFindDistance(world)) {
                        int newDist = getSlopeDistance(world, offset, distance + 1, facing.getOpposite());
                        if(newDist < shortestDistance) shortestDistance = newDist;
                    }
                }
            }
        }

        return shortestDistance;
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    private Set<EnumFacing> getPossibleFlowDirections(@Nonnull World world, @Nonnull BlockPos pos) {
        int prevSlopeDistance = 1000;
        final IBlockState here = world.getBlockState(pos);
        final Set<EnumFacing> flowDirections = new HashSet<>();

        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            if(canFluidFlow(world, pos, here, facing)) {
                BlockPos offset = pos.offset(facing);
                IBlockState neighbor = world.getBlockState(offset);
                FluidState fluidNeighbor = getFluidState(world, offset, neighbor);

                if(!isBlocked(world, offset, neighbor) && (!isCompatibleFluid(fluidNeighbor.getFluid(), getFluid()) || fluidNeighbor.getLevel() > 0)) {
                    int slopeDistance = 0;

                    if(isBlocked(world, offset.down(), world.getBlockState(offset.down()))) {
                        slopeDistance = getSlopeDistance(world, offset, 1, facing.getOpposite());
                    }

                    if(slopeDistance <= prevSlopeDistance) {
                        if(slopeDistance != prevSlopeDistance) flowDirections.clear();

                        flowDirections.add(facing);
                        prevSlopeDistance = slopeDistance;
                    }
                }
            }
        }

        return flowDirections;
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite
    private boolean canFlowInto(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        return !isBlocked(world, pos, state) && !isCompatibleFluid(getFluidState(world, pos, state).getFluid(), getFluid());
    }

    @Shadow
    protected abstract void placeStaticBlock(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState currentState);

    @Shadow
    protected abstract int getSlopeFindDistance(@Nonnull World worldIn);

    @Shadow
    protected abstract boolean isBlocked(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state);
}
