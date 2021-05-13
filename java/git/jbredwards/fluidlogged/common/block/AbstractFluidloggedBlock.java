package git.jbredwards.fluidlogged.common.block;

import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Field;
import java.util.Random;

/**
 * for blocks that are always fluidlogged (like kelp)
 * blocks that extend this should not also extend IFluidloggable
 * @author jbred
 *
 */
@SuppressWarnings("NullableProblems")
public abstract class AbstractFluidloggedBlock extends BlockFluidClassic
{
    //private or protected fields used by this class
    public static final Field canCreateSourcesField = ObfuscationReflectionHelper.findField(BlockFluidClassic.class, "canCreateSources");
    public static final Field quantaPerBlockField = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaPerBlock");
    static { canCreateSourcesField.setAccessible(true); }
    static { quantaPerBlockField.setAccessible(true); }

    //makes definedFluid public
    public final Fluid fluid;

    protected AbstractFluidloggedBlock(Fluid fluid, Material material, MapColor mapColor) {
        super(fluid, material, mapColor);
        this.fluid = definedFluid;

        //allows this to create sources if the parent fluid can
        if(fluid.getBlock() instanceof BlockFluidClassic) {
            try { canCreateSources = canCreateSourcesField.getBoolean(fluid.getBlock()); }
            catch(Exception e) { canCreateSources = false; }
        }
        else canCreateSources = (material == Material.WATER);
    }

    protected AbstractFluidloggedBlock(Fluid fluid, Material material) {
        super(fluid, material);
        this.fluid = definedFluid;

        //allows this to create sources if the parent fluid can
        if(fluid.getBlock() instanceof BlockFluidClassic) {
            try { canCreateSources = canCreateSourcesField.getBoolean(fluid.getBlock()); }
            catch(Exception e) { canCreateSources = false; }
        }
        else canCreateSources = (material == Material.WATER);
    }

    //updates this block's quanta values to match its parent's
    private boolean isQuantaDirty = true;
    protected final void updateQuanta() {
        if(isQuantaDirty) {
            try { setQuantaPerBlock(quantaPerBlockField.getInt(fluid.getBlock())); }
            catch(Exception ignored) {}

            isQuantaDirty = false;
        }
    }

    //return true if the side should flow (used to mimic a vanilla 1.13 waterlogged mechanic where if a block's side is solid, water doesn't flow from it)
    //this doe not serve the same function as canFlowInto() or canDisplace()
    public boolean canSideFlow(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing side) {
        return state.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    //no longer gets the fluid from the registery, now returns the fluid directly
    @Override
    public Fluid getFluid() {
        return fluid;
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
    public boolean canCollideCheck(@Nonnull IBlockState state, boolean fullHit) {
        return true;
    }

    @Nonnull
    @Override
    public Item getItemDropped(@Nonnull IBlockState state, @Nonnull Random rand, int fortune) {
        return Item.getItemFromBlock(this);
    }

    @Override
    public int quantityDropped(@Nonnull Random par1Random) {
        return 1;
    }

    @Override
    public boolean isReplaceable(IBlockAccess worldIn, BlockPos pos) {
        return false;
    }

    @Override
    public void onBlockExploded(World world, BlockPos pos, Explosion explosion) {
        world.setBlockState(pos, fluid.getBlock().getDefaultState(), 11);
        fluid.getBlock().onBlockDestroyedByExplosion(world, pos, explosion);
    }

    @Override
    public void onBlockDestroyedByPlayer(World worldIn, BlockPos pos, IBlockState state) {
        worldIn.setBlockState(pos, fluid.getBlock().getDefaultState(), 11);
    }

    @Override
    public boolean isSourceBlock(IBlockAccess world, BlockPos pos) {
        return true;
    }

    @SuppressWarnings("deprecation")
    @Nonnull
    @Override
    public IBlockState getStateFromMeta(int meta) {
        return getDefaultState();
    }

    @Override
    public int getMetaFromState(@Nonnull IBlockState state) {
        return 0;
    }

    @Override
    public boolean canRenderInLayer(IBlockState state, BlockRenderLayer layer) {
        return renderLayer == layer || fluid.getBlock().canRenderInLayer(fluid.getBlock().getDefaultState(), layer);
    }

    //so this doesn't create more of itself
    @Override
    protected void flowIntoBlock(World world, BlockPos pos, int meta) {
        if(meta < 0) return;
        if(displaceIfPossible(world, pos)) {
            final Block fluidBlock = (fluid.getBlock() instanceof BlockLiquid ? BlockLiquid.getFlowingBlock(blockMaterial) : fluid.getBlock());
            world.setBlockState(pos, fluidBlock.getDefaultState().withProperty(LEVEL, meta));
        }
    }

    @Override
    protected boolean canFlowInto(IBlockAccess world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        final boolean flag = state.getBlock() instanceof BlockLiquid && state.getValue(BlockLiquid.LEVEL) == 0;
        final boolean flag2 = state.getBlock() instanceof IFluidBlock && state.getValue(LEVEL) == 0;
        final boolean flag3 = (state.getBlock() instanceof IFluidBlock && ((IFluidBlock)state.getBlock()).getFluid() == fluid);
        final boolean flag4 = (fluid.getBlock() instanceof BlockLiquid ? state.getMaterial() == blockMaterial : flag3);

        return !flag && !flag2 && (flag4 || canDisplace(world, pos));
    }

    //prevents random currents
    @Override
    public Vec3d getFlowVector(IBlockAccess world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        int speedX = 0;
        int speedZ = 0;

        for(EnumFacing face : EnumFacing.HORIZONTALS) {
            if(canSideFlow(state, world, pos, face) && canFlowInto(world, pos.offset(face))) {
                speedX += face.getDirectionVec().getX();
                speedZ += face.getDirectionVec().getZ();
            }
        }

        return new Vec3d(speedX, super.getFlowVector(world, pos).y, speedZ);
    }

    //prevents this from flowing into other fluidlogged types
    @Override
    public boolean canDisplace(IBlockAccess world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        final Block block = state.getBlock();
        final Material material = state.getMaterial();

        if(block.isAir(state, world, pos)) return true;
        if(block == this) return false;
        if(displacements.containsKey(block)) return displacements.get(block);

        if(material.blocksMovement() || material == Material.PORTAL || material == Material.STRUCTURE_VOID) {
            return false;
        }

        final boolean replaceable = block.isReplaceable(world, pos);
        final int density = getDensity(world, pos);

        if(density == Integer.MAX_VALUE) return replaceable;
        else return replaceable && this.density > density;
    }

    //draining this block drops the item
    @Nullable
    @Override
    public FluidStack drain(World world, BlockPos pos, boolean doDrain) {
        if(doDrain) {
            final IBlockState state = world.getBlockState(pos);

            dropBlockAsItem(world, pos, state, 0);
            world.playEvent(Constants.WorldEvents.BREAK_BLOCK_EFFECTS, pos, Block.getStateId(state));
            world.setBlockToAir(pos);
        }

        return stack.copy();
    }

    @Override
    public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
        updateQuanta();

        final boolean canFlowVertical = canSideFlow(state, world, pos, densityDir < 0 ? EnumFacing.DOWN : EnumFacing.UP);

        //Flow vertically if possible
        if(canFlowVertical && canDisplace(world, pos.up(densityDir))) {
            flowIntoBlock(world, pos.up(densityDir), 1);
            return;
        }

        //Flow outward if possible
        if(1 >= quantaPerBlock) return;
        if(!canFlowVertical || !isFlowingVertically(world, pos)) {
            final boolean[] flowTo = getOptimalFlowDirections(world, pos);

            for(int i = 0; i < 4; i++) {
                int meta = !canSideFlow(state, world, pos, SIDES.get(i)) ? -1 : 1;
                if(flowTo[i]) flowIntoBlock(world, pos.offset(SIDES.get(i)), meta);
            }
        }
    }

    //fixes small issue with corners
    @Nonnull
    @Override
    public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        updateQuanta();

        IExtendedBlockState state = (IExtendedBlockState)super.getExtendedState(oldState, world, pos);
        state = state.withProperty(LEVEL_CORNERS[0], 1.0f);
        state = state.withProperty(LEVEL_CORNERS[1], 0.75f);
        state = state.withProperty(LEVEL_CORNERS[2], 0.5f);
        state = state.withProperty(LEVEL_CORNERS[3], 0.25f);

        return state;
    }

    @Override
    public boolean removedByPlayer(IBlockState state, World world, BlockPos pos, EntityPlayer player, boolean willHarvest) {
        onBlockHarvested(world, pos, state, player);
        return world.setBlockState(pos, fluid.getBlock().getDefaultState(), world.isRemote ? 11 : 3);
    }

    //================================================================
    //METHODS BELOW THIS EXIST TO MIMIC PROPERTIES OF THE PARENT FLUID
    //================================================================

    @Override
    public float getFilledPercentage(IBlockAccess world, BlockPos pos) {
        if(!(fluid.getBlock() instanceof BlockFluidBase)) return super.getFilledPercentage(world, pos);
        else return ((BlockFluidBase)fluid.getBlock()).getFilledPercentage(world, pos);
    }

    @SideOnly(Side.CLIENT)
    @Override
    public Vec3d getFogColor(World world, BlockPos pos, IBlockState state, Entity entity, Vec3d originalColor, float partialTicks) {
        return fluid.getBlock().getFogColor(world, pos, fluid.getBlock().getDefaultState(), entity, originalColor, partialTicks);
    }

    @SideOnly(Side.CLIENT)
    @Override
    public void randomDisplayTick(IBlockState stateIn, World worldIn, BlockPos pos, Random rand) {
        fluid.getBlock().randomDisplayTick(fluid.getBlock().getDefaultState(), worldIn, pos, rand);
    }

    @Override
    public void onEntityCollidedWithBlock(World worldIn, BlockPos pos, IBlockState state, Entity entityIn) {
        fluid.getBlock().onEntityCollidedWithBlock(worldIn, pos, fluid.getBlock().getDefaultState(), entityIn);
    }

    @Nullable
    @Override
    public Boolean isEntityInsideMaterial(IBlockAccess world, BlockPos blockpos, IBlockState iblockstate, Entity entity, double yToTest, Material materialIn, boolean testingHead) {
        return fluid.getBlock().isEntityInsideMaterial(world, blockpos, fluid.getBlock().getDefaultState(), entity, yToTest, materialIn, testingHead);
    }

    @Override
    public boolean isFireSource(World world, BlockPos pos, EnumFacing side) {
        return fluid.getBlock().isFireSource(world, pos, side);
    }

    @Override
    public int getFlammability(IBlockAccess world, BlockPos pos, EnumFacing face) {
        return fluid.getBlock().getFlammability(world, pos, face);
    }

    @Override
    public boolean isFlammable(IBlockAccess world, BlockPos pos, EnumFacing face) {
        return fluid.getBlock().isFlammable(world, pos, face);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int getLightValue(IBlockState state) {
        return fluid.getBlock().getDefaultState().getLightValue();
    }

    @SuppressWarnings("deprecation")
    @Override
    public int getLightOpacity(IBlockState state) {
        return fluid.getBlock().getDefaultState().getLightOpacity();
    }
}
