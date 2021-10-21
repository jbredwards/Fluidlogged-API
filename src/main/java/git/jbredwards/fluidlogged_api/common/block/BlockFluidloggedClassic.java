package git.jbredwards.fluidlogged_api.common.block;

import git.jbredwards.fluidlogged_api.common.util.FluidloggedAccessorUtils;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.item.Item;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.BlockFluidClassic;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Random;

/**
 * for blocks that are always fluidlogged (like kelp)
 * this class exists to try to reduce the capability usage as much as possible
 * blocks that extend this should not also extend IFluidloggable
 * @author jbred
 *
 */
public abstract class BlockFluidloggedClassic extends BlockFluidClassic implements IFluidloggableBase
{
    public BlockFluidloggedClassic(@Nonnull Fluid fluid, @Nonnull Material material, @Nonnull MapColor mapColor) {
        super(fluid, material, mapColor);
        canCreateSources = FluidloggedAccessorUtils.canCreateSources(fluid);
    }

    public BlockFluidloggedClassic(@Nonnull Fluid fluid, @Nonnull Material material) {
        this(fluid, material, material.getMaterialMapColor());
    }

    //sync some fluid variables during runtime (since mods like to set these at different times?)
    protected boolean isSyncDirty = true;
    protected void sync(@Nonnull Fluid fluid) {
        if(isSyncDirty && fluid.getBlock() instanceof BlockFluidClassic) {
            quantaPerBlock      = FluidloggedAccessorUtils.quantaPerBlock(fluid);
            quantaPerBlockFloat = FluidloggedAccessorUtils.quantaPerBlockFloat(fluid);
            quantaFraction      = FluidloggedAccessorUtils.quantaFraction(fluid);
            canCreateSources    = FluidloggedAccessorUtils.canCreateSources(fluid);
        }

        isSyncDirty = false;
    }

    @Nonnull
    @Override
    public BlockFaceShape getBlockFaceShape(@Nonnull IBlockAccess worldIn, @Nonnull IBlockState state, @Nonnull BlockPos pos, @Nonnull EnumFacing face) {
        return BlockFaceShape.SOLID;
    }

    @Override
    public AxisAlignedBB getCollisionBoundingBox(@Nonnull IBlockState blockState, @Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) {
        return FULL_BLOCK_AABB;
    }

    @Override
    public boolean canCollideCheck(@Nonnull IBlockState state, boolean fullHit) { return true; }

    @Nonnull
    @Override
    public Item getItemDropped(@Nonnull IBlockState state, @Nonnull Random rand, int fortune) {
        return Item.getItemFromBlock(this);
    }

    @Override
    public int quantityDropped(@Nonnull Random rand) { return 1; }

    @Override
    public boolean isReplaceable(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) { return false; }

    @Nonnull
    @Override
    public IBlockState getStateFromMeta(int meta) { return getDefaultState(); }

    @Override
    public int getMetaFromState(@Nonnull IBlockState state) { return 0; }

    @Override
    public boolean canDrain(@Nonnull World world, @Nonnull BlockPos pos) {
        return world.getBlockState(pos).getBlock().isReplaceable(world, pos);
    }

    @Nullable
    @Override
    public FluidStack drain(@Nonnull World world, @Nonnull BlockPos pos, boolean doDrain) {
        return canDrain(world, pos) ? super.drain(world, pos, doDrain) : null;
    }

    //============================================================
    //CLONED FROM PARENT FLUID. CHANGING THESE ARE NOT RECOMMENDED
    //============================================================
}
