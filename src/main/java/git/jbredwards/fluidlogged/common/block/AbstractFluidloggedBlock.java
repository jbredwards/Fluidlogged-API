package git.jbredwards.fluidlogged.common.block;

import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.BlockSnow;
import net.minecraft.block.SoundType;
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
import net.minecraft.util.math.MathHelper;
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
    public static final Field nextStepDistance = ObfuscationReflectionHelper.findField(Entity.class, "field_70150_b");
    static {
        canCreateSourcesField.setAccessible(true);
        quantaPerBlockField.setAccessible(true);
        nextStepDistance.setAccessible(true);
    }

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
        final boolean flag = state.getBlock() instanceof BlockLiquid && state.getValue(LEVEL) == 0;
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

    //fixes small issues with corners
    @Nonnull
    @Override
    public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        updateQuanta();

        IExtendedBlockState state = (IExtendedBlockState)oldState;
        state = state.withProperty(FLOW_DIRECTION, (float)getFlowDirection(world, pos));

        final IBlockState[][] upBlockState = new IBlockState[3][3];
        upBlockState[1][1] = world.getBlockState(pos.down(densityDir));

        final float[][] corner = new float[2][2];
        final float[][] height = new float[3][3];
        height[1][1] = getFluidHeightForRender(world, pos, upBlockState[1][1]);

        //checks if the block above this is a fluid block
        if(height[1][1] == 1) {
            //updates the corner values
            for(int i = 0; i < 2; i++) {
                for(int j = 0; j < 2; j++) {
                    corner[i][j] = 1;
                }
            }
        }
        //else
        else {
            //height for sides & corners
            for(int i = 0; i < 3; i++) {
                for(int j = 0; j < 3; j++) {
                    if(i != 1 || j != 1) {
                        upBlockState[i][j] = world.getBlockState(pos.add(i - 1, 0, j - 1).down(densityDir));
                        height[i][j] = getFluidHeightForRender(world, pos.add(i - 1, 0, j - 1), upBlockState[i][j]);
                    }
                }
            }
            //updates corners
            for(int i = 0; i < 2; i++) {
                for(int j = 0; j < 2; j++) {
                    corner[i][j] = getFluidHeightAverage(height[i][j], height[i][j + 1], height[i + 1][j], height[i + 1][j + 1]);
                }
            }

            //check for down flow above corners
            boolean n =  isFluid(upBlockState[0][1]);
            boolean s =  isFluid(upBlockState[2][1]);
            boolean w =  isFluid(upBlockState[1][0]);
            boolean e =  isFluid(upBlockState[1][2]);
            boolean nw = isFluid(upBlockState[0][0]);
            boolean ne = isFluid(upBlockState[0][2]);
            boolean sw = isFluid(upBlockState[2][0]);
            boolean se = isFluid(upBlockState[2][2]);

            if(nw || n || w) corner[0][0] = 1;
            if(ne || n || e) corner[0][1] = 1;
            if(sw || s || w) corner[1][0] = 1;
            if(se || s || e) corner[1][1] = 1;
        }

        //fixes corners
        if(height[1][1] != 1 || !canSideFlow(oldState, world, pos, densityDir == 1 ? EnumFacing.UP : EnumFacing.DOWN)) {
            boolean n = !canSideFlow(oldState, world, pos, EnumFacing.NORTH);
            boolean s = !canSideFlow(oldState, world, pos, EnumFacing.SOUTH);
            boolean w = !canSideFlow(oldState, world, pos, EnumFacing.WEST);
            boolean e = !canSideFlow(oldState, world, pos, EnumFacing.EAST);
            boolean nFull = isFullFluid(world, pos.offset(EnumFacing.NORTH));
            boolean sFull = isFullFluid(world, pos.offset(EnumFacing.SOUTH));
            boolean eFull = isFullFluid(world, pos.offset(EnumFacing.EAST));
            boolean wFull = isFullFluid(world, pos.offset(EnumFacing.WEST));

            if((n || w) && !nFull && !wFull) corner[0][0] = quantaFraction;
            if((s || w) && !sFull && !wFull) corner[0][1] = quantaFraction;
            if((n || e) && !nFull && !eFull) corner[1][0] = quantaFraction;
            if((s || e) && !sFull && !eFull) corner[1][1] = quantaFraction;
        }

        //side overlays
        for(int i = 0; i < 4; i++) {
            EnumFacing side = EnumFacing.getHorizontal(i);
            BlockPos offset = pos.offset(side);
            boolean useOverlay = world.getBlockState(offset).getBlockFaceShape(world, offset, side.getOpposite()) == BlockFaceShape.SOLID;
            state = state.withProperty(SIDE_OVERLAYS[i], useOverlay);
        }

        state = state.withProperty(LEVEL_CORNERS[0], corner[0][0]);
        state = state.withProperty(LEVEL_CORNERS[1], corner[0][1]);
        state = state.withProperty(LEVEL_CORNERS[2], corner[1][1]);
        state = state.withProperty(LEVEL_CORNERS[3], corner[1][0]);

        return state;
    }

    //same as the BlockFluidBase version, but with protected access rather than private
    protected static boolean isFluid(IBlockState state) {
        return state.getMaterial().isLiquid() || state.getBlock() instanceof IFluidBlock;
    }

    //used by getExtendedState()
    protected static boolean isFullFluid(IBlockAccess world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);

        if(!isFluid(state)) return false;
        else {
            if(state.getBlock() instanceof BlockFluidClassic) return !((BlockFluidClassic)state.getBlock()).isSourceBlock(world, pos);
            else return state.getBlock() instanceof BlockFluidFinite || state.getBlock().getMetaFromState(state) != 0;
        }
    }

    @Override
    public boolean removedByPlayer(IBlockState state, World world, BlockPos pos, EntityPlayer player, boolean willHarvest) {
        onBlockHarvested(world, pos, state, player);
        return world.setBlockState(pos, fluid.getBlock().getDefaultState(), world.isRemote ? 11 : 3);
    }

    //walk sound fix
    @Override
    public void onEntityWalk(World worldIn, BlockPos pos, Entity entityIn) {
        try {
            //if the entity is above the fluid
            if(entityIn.posY - (int)entityIn.posY < 1) {
                //distance the player moved x & z
                final double x = entityIn.posX - entityIn.prevPosX;
                final double z = entityIn.posZ - entityIn.prevPosZ;

                //if the cooldown is over
                if(entityIn.distanceWalkedOnStepModified + MathHelper.sqrt(x * x + z * z) * 0.6D > nextStepDistance.getInt(entityIn) ) {
                    SoundType type = getSoundType(worldIn.getBlockState(pos), worldIn, pos, entityIn);

                    //special case for snow
                    final IBlockState up = worldIn.getBlockState(pos.up());
                    if(up.getBlock() instanceof BlockSnow) type = up.getBlock().getSoundType(up, worldIn, pos.up(), entityIn);
                    //plays the step sound
                    entityIn.playSound(type.getStepSound(), type.getVolume() * 0.15f, type.getPitch());
                }
            }
        } catch (IllegalAccessException ignored) {}
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

    @SideOnly(Side.CLIENT)
    @Override
    public int getPackedLightmapCoords(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return fluid.getBlock().getDefaultState().getPackedLightmapCoords(world, pos);
    }

    @Override
    public boolean isPassable(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return fluid.getBlock().isPassable(world, pos);
    }
}
