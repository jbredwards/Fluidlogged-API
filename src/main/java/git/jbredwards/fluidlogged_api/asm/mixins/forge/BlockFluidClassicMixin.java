package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.BlockFluidClassic;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.Random;

import static git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockFluidClassic.class)
public abstract class BlockFluidClassicMixin extends BlockFluidBaseMixin implements IFluidloggableFluid
{
    @Shadow(remap = false)
    protected boolean canCreateSources;

    public BlockFluidClassicMixin(@Nonnull Material materialIn) { super(materialIn); }

    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite(remap = false)
    public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
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
                for(EnumFacing side : EnumFacing.HORIZONTALS)
                    maxQuanta = getLargerQuanta(world, pos.offset(side), maxQuanta);

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

        final IBlockState here = world.getBlockState(pos); //fluidlogged fluids will have a different state here than the state input

        // Flow vertically if possible
        if(canFluidFlow(world, pos, here, facingDir.getOpposite()) && canDisplace(world, pos.up(densityDir))) {
            flowIntoBlock(world, pos.up(densityDir), 1);
            return;
        }

        // Flow outward if possible
        int flowMeta = quantaPerBlock - quantaRemaining + 1;
        if(flowMeta >= quantaPerBlock) return;


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
        if(facing != null && canFluidFlow(world, pos, here, facing)) return false;

        final FluidState fluidState = getFluidState(world, pos, here);
        return isCompatibleFluid(fluidState.getFluid(), getFluid()) && fluidState.getLevel() == 0;
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
    public int getQuantaValue(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        if(!isCompatibleFluid(fluidState.getFluid(), getFluid())) return -1;
        else return quantaPerBlock - fluidState.getLevel();
    }

    @Override
    public boolean isFluidloggableFluid(@Nonnull IBlockState fluid, boolean checkLevel) {
        if(hasTileEntity(fluid) || this != getFluid().getBlock()) return false;
        else if(checkLevel) return true;

        final int level = fluid.getValue(BlockLiquid.LEVEL);
        return level == 0 || level >= 8 && canCreateSources;
    }

    @Shadow
    protected abstract int getLargerQuanta(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare);

    @Shadow
    protected abstract void flowIntoBlock(@Nonnull World world, @Nonnull BlockPos pos, int meta);
}
