package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block;

import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.asm.plugins.ASMHooks;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.BlockDynamicLiquid;
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
import net.minecraftforge.fluids.BlockFluidBase;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

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
    private static final List<EnumFacing> SIDES = Collections.unmodifiableList(Arrays.asList(
            EnumFacing.WEST, EnumFacing.EAST, EnumFacing.NORTH, EnumFacing.SOUTH));

    private static Map<Block, Boolean> displacements = null;
    private final boolean[] isOptimalFlowDirection = new boolean[4];

    protected BlockDynamicLiquidMixin(@Nonnull Material materialIn) { super(materialIn); }

    private Map<Block, Boolean> displacements() {
        if(displacements == null) displacements = ASMHooks.defaultDisplacements(new HashMap<>());
        return displacements;
    }

    /**
     * @reason rewrite vanilla's fluid logic to match forge's
     * @author jbred
     */
    @Overwrite
    public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
        final IBlockState here = world.getBlockState(pos); //fluidlogged fluids will have a different state here than the state input
        final int lavaDif = (blockMaterial == Material.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
        int quantaRemaining = 8 - state.getValue(BlockLiquid.LEVEL);

        // check adjacent block levels if non-source
        if(quantaRemaining < 8) {
            int adjacentSourceBlocks = 0;
            final int expQuanta;

            if(ForgeEventFactory.canCreateFluidSource(world, pos, state, blockMaterial == Material.WATER)) {
                for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                    BlockPos offset = pos.offset(facing);

                    if(isSourceBlock(world, offset, world.getBlockState(offset), facing.getOpposite()))
                        adjacentSourceBlocks++;
                }
            }

            // new source block
            final IBlockState vertical = world.getBlockState(pos.down());
            if(adjacentSourceBlocks >= 2 && (vertical.getMaterial().isSolid() || isSourceBlock(world, pos.down(), vertical, EnumFacing.UP)))
                expQuanta = 8;

            // vertical flow into block
            else if(hasVerticalFlow(world, pos)) expQuanta = 8 - lavaDif;

            else {
                int maxQuanta = -100;
                for(EnumFacing side : EnumFacing.HORIZONTALS) {
                    BlockPos offset = pos.offset(side);

                    if(canFluidFlow(world, pos, here, side) && canFluidFlow(world, offset, world.getBlockState(offset), side.getOpposite()))
                        maxQuanta = getLargerQuanta(world, offset, maxQuanta);
                }

                expQuanta = maxQuanta - lavaDif;
            }

            // decay calculation
            if(expQuanta != quantaRemaining) {
                quantaRemaining = expQuanta;

                if(expQuanta <= 0) world.setBlockToAir(pos);
                else {
                    world.setBlockState(pos, state.withProperty(BlockLiquid.LEVEL, 8 - expQuanta), Constants.BlockFlags.SEND_TO_CLIENTS);
                    world.scheduleUpdate(pos, this, tickRate(world));
                    world.notifyNeighborsOfStateChange(pos, this, false);
                }
            }
        }

        // Flow vertically if possible
        if(canFluidFlow(world, pos, here, EnumFacing.DOWN) && canDisplace(world, pos.down())) {
            flowIntoBlock(world, pos.down(), lavaDif);
            return;
        }

        // Flow outward if possible
        int flowMeta = 8 - quantaRemaining + lavaDif;
        if(flowMeta >= 8) return;

        if(isSourceBlock(world, pos, here, null) || !isFlowingVertically(world, pos)) {
            if(hasVerticalFlow(world, pos)) flowMeta = lavaDif;

            final boolean[] flowTo = getOptimalFlowDirections(world, pos, here);
            for(int i = 0; i < 4; i++)
                if(flowTo[i] && canFluidFlow(world, pos, here, SIDES.get(i)))
                    flowIntoBlock(world, pos.offset(SIDES.get(i)), flowMeta);
        }
    }

    private boolean canDisplace(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return true;
        //checks if this & a fluid here are the same
        else if(isCompatibleFluid(getFluid(), getFluidState(world, pos, state).getFluid())) return false;
        //predefined displacements
        else if(displacements().containsKey(state.getBlock())) return displacements.get(state.getBlock());

        final Material material = state.getMaterial();
        if(material.blocksMovement() || material == Material.PORTAL || material == Material.STRUCTURE_VOID) return false;

        final int density = BlockFluidBase.getDensity(world, pos);
        return density == Integer.MAX_VALUE || getFluid().getDensity() > density;
    }

    private boolean isFlowingVertically(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState here = world.getBlockState(pos);
        if(!canFluidFlow(world, pos, here, EnumFacing.DOWN)) return false;

        final IBlockState neighbor = world.getBlockState(pos.down());
        return isCompatibleFluid(getFluidState(world, pos.down(), neighbor).getFluid(), getFluid())
                || (isCompatibleFluid(getFluidState(world, pos, here).getFluid(), getFluid())
                && canFlowInto(world, pos.down()));
    }

    private boolean isSourceBlock(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return isSourceBlock(world, pos, world.getBlockState(pos), null);
    }

    private boolean isSourceBlock(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nullable EnumFacing facing) {
        if(facing != null && !canFluidFlow(world, pos, here, facing)) return false;

        final FluidState fluidState = getFluidState(world, pos, here);
        return isCompatibleFluid(fluidState.getFluid(), getFluid()) && fluidState.getLevel() == 0;
    }

    private boolean canFlowInto(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return isCompatibleFluid(getFluidState(world, pos).getFluid(), getFluid()) || canDisplace(world, pos);
    }

    private int getLargerQuanta(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare) {
        int quantaRemaining = getEffectiveQuanta(world, pos);
        if(quantaRemaining <= 0) return compare;

        return Math.max(quantaRemaining, compare);
    }

    private boolean[] getOptimalFlowDirections(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
        final int lavaDif = (blockMaterial == Material.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
        final int[] flowCost = { 1000, 1000, 1000, 1000 };

        for(int side = 0; side < 4; side++) {
            if(!canFluidFlow(world, pos, here, SIDES.get(side))) continue;

            BlockPos offset = pos.offset(SIDES.get(side));
            if(!canFlowInto(world, offset) || isSourceBlock(world, offset)) continue;

            if(canFlowInto(world, offset.down())) flowCost[side] = 0;
            else flowCost[side] = calculateFlowCost(world, offset, lavaDif, side);
        }

        int min = Ints.min(flowCost);
        for(int side = 0; side < 4; side++) isOptimalFlowDirection[side] = (flowCost[side] == min);

        return isOptimalFlowDirection;
    }

    private int calculateFlowCost(@Nonnull World world, @Nonnull BlockPos pos, int recurseDepth, int side) {
        final int lavaDif = (blockMaterial == Material.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
        int cost = 1000;

        for(int adjSide = 0; adjSide < 4; adjSide++) {
            if(SIDES.get(adjSide) == SIDES.get(side).getOpposite()) continue;
            BlockPos pos2 = pos.offset(SIDES.get(adjSide));

            if(!canFlowInto(world, pos2) || isSourceBlock(world, pos2)) continue;
            else if(canFlowInto(world, pos2.down())) return recurseDepth;
            else if(recurseDepth >= 4) continue;

            cost = Math.min(cost, calculateFlowCost(world, pos2, recurseDepth + lavaDif, adjSide));
        }

        return cost;
    }

    private void flowIntoBlock(@Nonnull World world, @Nonnull BlockPos pos, int meta) {
        if(meta >= 0 && displaceIfPossible(world, pos))
            world.setBlockState(pos, BlockLiquid.getFlowingBlock(blockMaterial)
                    .getDefaultState().withProperty(BlockLiquid.LEVEL, meta));
    }

    private boolean displaceIfPossible(@Nonnull World world, @Nonnull BlockPos pos) {
        boolean canDisplace = canDisplace(world, pos);
        if(canDisplace) {
            final IBlockState state = world.getBlockState(pos);
            final Block block = state.getBlock();

            if(!block.isAir(state, world, pos) && getFluidFromBlock(block) == null) {
                // Forge: Vanilla has a 'bug' where snowballs don't drop like every other block. So special case because ewww...
                if(block != Blocks.SNOW_LAYER) block.dropBlockAsItem(world, pos, state, 0);
            }
        }

        return canDisplace;
    }

    @Override
    public void neighborChanged(@Nonnull IBlockState state, @Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull Block blockIn, @Nonnull BlockPos fromPos) {
        if(!checkForMixing(worldIn, pos, state)) worldIn.scheduleUpdate(pos, this, tickRate(worldIn));
    }

    @Override
    public boolean requiresUpdates() { return false; }
}
