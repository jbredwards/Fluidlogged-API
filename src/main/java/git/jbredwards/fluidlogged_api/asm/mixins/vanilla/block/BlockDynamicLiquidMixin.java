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
