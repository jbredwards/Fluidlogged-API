package git.jbredwards.fluidlogged_api.common.block;

import git.jbredwards.fluidlogged_api.common.util.FluidloggedAccessorUtils;
import net.minecraft.block.Block;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.item.Item;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.common.util.Constants;
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
        sync(fluid.getBlock(), false);
    }

    public BlockFluidloggedClassic(@Nonnull Fluid fluid, @Nonnull Material material) {
        this(fluid, material, material.getMaterialMapColor());
    }

    //sync some fluid variables during runtime (since mods like to set these at different times?)
    protected boolean isSyncDirty = true;
    protected void sync(@Nullable Block fluid, boolean remapDirty) {
        if(isSyncDirty && fluid instanceof BlockFluidClassic) {
            quantaPerBlock      = FluidloggedAccessorUtils.quantaPerBlock(fluid);
            quantaPerBlockFloat = FluidloggedAccessorUtils.quantaPerBlockFloat(fluid);
            quantaFraction      = FluidloggedAccessorUtils.quantaFraction(fluid);
            canCreateSources    = FluidloggedAccessorUtils.canCreateSources(fluid);
        }

        if(remapDirty) isSyncDirty = false;
    }

    @Nonnull
    @Override
    protected BlockStateContainer createBlockState() { return blockStateBuilder().build(); }

    //exists so you don't have to add the annoying fluid properties for every block that extends this, yay
    protected BlockStateContainer.Builder blockStateBuilder() {
        return new BlockStateContainer.Builder(this).add(LEVEL).add(FLUID_RENDER_PROPS.toArray(new IUnlistedProperty<?>[0]));
    }

    //fluids for IFluidloggableBase blocks are rendered through RenderChunkPlugin, and should not be managed through this method
    @Nonnull
    @Override
    public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) { return oldState; }

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

    @Override
    public boolean isSourceBlock(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) { return true; }

    @Nullable
    @Override
    public FluidStack drain(@Nonnull World world, @Nonnull BlockPos pos, boolean doDrain) {
        if(canDrain(world, pos) && isSourceBlock(world, pos)) {
            if(doDrain) {
                final IBlockState state = world.getBlockState(pos);

                world.playEvent(Constants.WorldEvents.BREAK_BLOCK_EFFECTS, pos, Block.getStateId(state));
                state.getBlock().dropBlockAsItem(world, pos, state, 0);
                world.setBlockState(pos, Blocks.AIR.getDefaultState());
            }

            return stack.copy();
        }
        //default
        return null;
    }

    //============================================================
    //CLONED FROM PARENT FLUID. CHANGING THESE ARE NOT RECOMMENDED
    //============================================================
}
