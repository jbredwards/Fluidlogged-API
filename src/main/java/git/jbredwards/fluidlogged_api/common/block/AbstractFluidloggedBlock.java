package git.jbredwards.fluidlogged_api.common.block;

import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.asm.ASMHooks;
import git.jbredwards.fluidlogged_api.common.event.FluidloggedEvents;
import git.jbredwards.fluidlogged_api.util.FluidloggedUtils;
import mcp.MethodsReturnNonnullByDefault;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.item.Item;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nullable;
import javax.annotation.ParametersAreNonnullByDefault;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Random;

/**
 * for blocks that are always fluidlogged (like kelp)
 * blocks that extend this should not also extend IFluidloggable
 * @author jbred
 *
 */
@MethodsReturnNonnullByDefault
@ParametersAreNonnullByDefault
public abstract class AbstractFluidloggedBlock extends BlockFluidClassic
{
    //private or protected fields used by this class
    public static final Field canCreateSourcesField = ObfuscationReflectionHelper.findField(BlockFluidClassic.class, "canCreateSources");
    public static final Field quantaPerBlockField = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaPerBlock");
    public static final Field quantaPerBlockFloatField = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaPerBlockFloat");
    public static final Field quantaFractionField = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaFraction");
    static {
        canCreateSourcesField.setAccessible(true);
        quantaPerBlockField.setAccessible(true);
        quantaPerBlockFloatField.setAccessible(true);
        quantaFractionField.setAccessible(true);
    }
    //makes definedFluid public
    public Fluid fluid;

    public AbstractFluidloggedBlock(Fluid fluid, Material material, MapColor mapColor) {
        super(fluid, material, mapColor);
        this.canCreateSources = FluidloggedUtils.canFluidCreateSources(fluid);
        this.fluid = definedFluid;
    }

    public AbstractFluidloggedBlock(Fluid fluid, Material material) {
        super(fluid, material);
        this.canCreateSources = FluidloggedUtils.canFluidCreateSources(fluid);
        this.fluid = definedFluid;
    }

    //updates this block's quanta values to match its parent's
    public boolean isQuantaDirty = true;
    public void updateQuanta() {
        if(isQuantaDirty) {
            try {
                quantaPerBlock = quantaPerBlockField.getInt(fluid.getBlock());
                quantaPerBlockFloat = quantaPerBlockFloatField.getFloat(fluid.getBlock());
                quantaFraction = quantaFractionField.getFloat(fluid.getBlock());

                if(fluid.getBlock() instanceof BlockFluidClassic) {
                    canCreateSources = canCreateSourcesField.getBoolean(fluid.getBlock());
                }
            }
            catch(Exception ignored) {}

            isQuantaDirty = false;
        }
    }

    //return true if the side should flow (used to mimic a vanilla 1.13 waterlogged mechanic where if a block's side is solid, water doesn't flow from it)
    //this doe not serve the same function as canFlowInto() or canDisplace()
    public boolean canSideFlow(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing side) {
        return state.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    //returns true if the fluid should render
    @SideOnly(Side.CLIENT)
    public boolean shouldFluidRender(IBlockState state, IBlockAccess world, BlockPos pos) {
        return true;
    }

    @Override
    public BlockFaceShape getBlockFaceShape(IBlockAccess worldIn, IBlockState state, BlockPos pos, EnumFacing face) {
        return BlockFaceShape.SOLID;
    }

    @Override
    public AxisAlignedBB getCollisionBoundingBox(IBlockState blockState, IBlockAccess worldIn, BlockPos pos) {
        return FULL_BLOCK_AABB;
    }

    @Override
    public boolean canCollideCheck(IBlockState state, boolean fullHit) {
        return true;
    }

    @Override
    public Item getItemDropped(IBlockState state, Random rand, int fortune) {
        return Item.getItemFromBlock(this);
    }

    @Override
    public int quantityDropped(Random par1Random) {
        return 1;
    }

    @Override
    public boolean isReplaceable(IBlockAccess worldIn, BlockPos pos) {
        return false;
    }

    @Override
    public void onBlockExploded(World world, BlockPos pos, Explosion explosion) {
        final Block toCreate = fluid.getBlock() instanceof BlockLiquid ? BlockLiquid.getFlowingBlock(
                fluid.getBlock().getDefaultState().getMaterial()) : fluid.getBlock();
        world.setBlockState(pos, toCreate.getDefaultState(), 11);
        toCreate.onBlockDestroyedByExplosion(world, pos, explosion);
    }

    @Override
    public void onBlockDestroyedByPlayer(World worldIn, BlockPos pos, IBlockState state) {
        final Block toCreate = fluid.getBlock() instanceof BlockLiquid ? BlockLiquid.getFlowingBlock(
                fluid.getBlock().getDefaultState().getMaterial()) : fluid.getBlock();

        worldIn.setBlockState(pos, toCreate.getDefaultState(), 11);
    }

    @Override
    public boolean isSourceBlock(IBlockAccess world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);

        if(state.getBlock() == this) return true;
        else if(fluid != FluidloggedUtils.getFluidFromBlock(state.getBlock())) return false;
        else return FluidloggedUtils.isSource(world, pos, state);
    }

    @SuppressWarnings("deprecation")
    @Override
    public IBlockState getStateFromMeta(int meta) {
        return getDefaultState();
    }

    @Override
    public int getMetaFromState(IBlockState state) {
        return 0;
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
    public boolean canFlowInto(IBlockAccess world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        final boolean flag = state.getBlock() instanceof BlockLiquid && state.getValue(BlockLiquid.LEVEL) == 0;
        final boolean flag2 = state.getBlock() instanceof IFluidBlock && state.getValue(LEVEL) == 0;
        final boolean flag3 = (state.getBlock() instanceof IFluidBlock && ((IFluidBlock)state.getBlock()).getFluid() == fluid);
        final boolean flag4 = (fluid.getBlock() instanceof BlockLiquid ? state.getMaterial() == blockMaterial : flag3);

        return !flag && !flag2 && (flag4 || canDisplace(world, pos));
    }

    //prevents this from flowing into other fluidlogged types
    @Override
    public boolean canDisplace(IBlockAccess world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        final Block block = state.getBlock();
        final Material material = state.getMaterial();

        if(block.isAir(state, world, pos)) return true;
        if(ASMHooks.canDisplace(block, this, world, pos) == this) return false;
        if(displacements.containsKey(block)) return displacements.get(block);

        if(material.blocksMovement() || material == Material.PORTAL || material == Material.STRUCTURE_VOID) {
            return false;
        }

        final boolean replaceable = block.isReplaceable(world, pos) || FluidloggedUtils.isStateFluidloggable(state, fluid);
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
    public void updateTick(World world, BlockPos pos, IBlockState state, Random rand) {
        final boolean canFlowVertical = canSideFlow(state, world, pos, densityDir < 0 ? EnumFacing.DOWN : EnumFacing.UP);
        //Flow vertically if possible
        if(canFlowVertical && canDisplace(world, pos.up(densityDir))) {
            flowIntoBlock(world, pos.up(densityDir), 1);
            return;
        }
        //Flow outward if possible
        if(1 >= quantaPerBlock) return;
        final boolean[] flowTo = getOptimalFlowDirections(world, pos);
        for(int side = 0; side < 4; side++) {
            if(flowTo[side] && canSideFlow(state, world, pos, SIDES.get(side))) flowIntoBlock(world, pos.offset(SIDES.get(side)), 1);
        }
        //lava fluid fire spread
        if(fluid == FluidRegistry.LAVA) Blocks.LAVA.updateTick(world, pos, Blocks.LAVA.getDefaultState(), rand);
        //modded fluid fire spread
        else if(fluid.getBlock() instanceof IFluidFireSpreader) {
            ((IFluidFireSpreader)fluid.getBlock()).tryFireSpread(world, pos, state, rand, Arrays.stream(EnumFacing.values()).filter(e -> canSideFlow(state, world, pos, e)).toArray(EnumFacing[]::new));
        }
    }

    @Override
    protected boolean[] getOptimalFlowDirections(World world, BlockPos pos) {
        for(int side = 0; side < 4; side++) {
            BlockPos pos2 = pos.offset(SIDES.get(side));
            flowCost[side] = 1000;
            //can't flow into the block
            if(!canSideFlow(world.getBlockState(pos), world, pos, SIDES.get(side)) || !canFlowInto(world, pos2) || isSourceBlock(world, pos2)) continue;
            //can flow into the block
            if(canFlowInto(world, pos2.up(densityDir))) flowCost[side] = 0;
            else flowCost[side] = calculateFlowCost(world, pos2, 1, side);
        }

        final int min = Ints.min(flowCost);
        for(int side = 0; side < 4; side++) {
            isOptimalFlowDirection[side] = flowCost[side] == min;
        }

        return isOptimalFlowDirection;
    }

    @Override
    public boolean isFlowingVertically(IBlockAccess world, BlockPos pos) {
        final IBlockState up = world.getBlockState(pos.up(densityDir));
        if(fluid == FluidloggedUtils.getFluidFromBlock(up.getBlock())) return true;

        final IBlockState state = world.getBlockState(pos);
        if(fluid == FluidloggedUtils.getFluidFromBlock(state.getBlock())) return canFlowInto(world, pos.up(densityDir));

        //default
        return false;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public boolean shouldSideBeRendered(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing side) {
        if(!canSideFlow(state, world, pos, side)) return true;
        else return super.shouldSideBeRendered(state, world, pos, side);
    }

    //fixes small issues with corners
    @Override
    public IBlockState getExtendedState(IBlockState oldState, IBlockAccess world, BlockPos pos) {
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
            final boolean n =  ASMHooks.isFluid(upBlockState[0][1], this, world, densityDir, pos.north());
            final boolean s =  ASMHooks.isFluid(upBlockState[2][1], this, world, densityDir, pos.south());
            final boolean w =  ASMHooks.isFluid(upBlockState[1][0], this, world, densityDir, pos.west());
            final boolean e =  ASMHooks.isFluid(upBlockState[1][2], this, world, densityDir, pos.east());
            final boolean nw = ASMHooks.isFluid(upBlockState[0][0], this, world, densityDir, pos.north().west());
            final boolean ne = ASMHooks.isFluid(upBlockState[0][2], this, world, densityDir, pos.north().east());
            final boolean sw = ASMHooks.isFluid(upBlockState[2][0], this, world, densityDir, pos.south().west());
            final boolean se = ASMHooks.isFluid(upBlockState[2][2], this, world, densityDir, pos.south().east());

            if(nw || n || w) corner[0][0] = 1;
            if(ne || n || e) corner[0][1] = 1;
            if(sw || s || w) corner[1][0] = 1;
            if(se || s || e) corner[1][1] = 1;
        }

        //fixes corners
        if(height[1][1] != 1) {
            if(fixCorner(oldState, world, pos, EnumFacing.NORTH, EnumFacing.WEST) || fixCorner(oldState, world, pos, EnumFacing.WEST, EnumFacing.NORTH)) corner[0][0] = quantaFraction;
            if(fixCorner(oldState, world, pos, EnumFacing.SOUTH, EnumFacing.WEST) || fixCorner(oldState, world, pos, EnumFacing.WEST, EnumFacing.SOUTH)) corner[0][1] = quantaFraction;
            if(fixCorner(oldState, world, pos, EnumFacing.NORTH, EnumFacing.EAST) || fixCorner(oldState, world, pos, EnumFacing.EAST, EnumFacing.NORTH)) corner[1][0] = quantaFraction;
            if(fixCorner(oldState, world, pos, EnumFacing.SOUTH, EnumFacing.EAST) || fixCorner(oldState, world, pos, EnumFacing.EAST, EnumFacing.SOUTH)) corner[1][1] = quantaFraction;
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

    //used by this::getExtendedState
    public boolean fixCorner(IBlockState oldState, IBlockAccess world, BlockPos pos, EnumFacing primary, EnumFacing other) {
        if(canSideFlow(oldState, world, pos, primary)) return false;

        final BlockPos offset = pos.offset(other);
        final IBlockState neighbor = world.getBlockState(offset);
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(neighbor.getBlock());

        if(this.fluid != fluid) return true;
        else return neighbor.getBlock() instanceof AbstractFluidloggedBlock &&
                (!((AbstractFluidloggedBlock)neighbor.getBlock()).canSideFlow(neighbor, world, offset, primary)
                || !((AbstractFluidloggedBlock)neighbor.getBlock()).canSideFlow(neighbor, world, offset, other.getOpposite()));
    }

    //fixes the fluidlogged block flow direction
    @Override
    public Vec3d getFlowVector(IBlockAccess world, BlockPos pos) {
        final int decay = ASMHooks.getFlowDecay(this, world, pos, null, fluid, quantaPerBlock, densityDir);
        Vec3d vec = new Vec3d(0, 0, 0);

        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
            BlockPos offset = pos.offset(facing);
            IBlockState state = world.getBlockState(offset);

            if(canSideFlow(state, world, pos, facing)) {
                int otherDecay = ASMHooks.getFlowDecay(this, world, offset, facing, fluid, quantaPerBlock, densityDir);
                if(otherDecay >= quantaPerBlock) {
                    if(!ASMHooks.getFlowVector(world, offset, facing, fluid)) {
                        otherDecay = ASMHooks.getFlowDecay(this, world, offset.up(densityDir), null, fluid, quantaPerBlock, densityDir);
                        if(otherDecay < quantaPerBlock) {
                            int power = otherDecay - (decay - quantaPerBlock);
                            vec = vec.addVector(facing.getFrontOffsetX() * power, 0, facing.getFrontOffsetZ() * power);
                        }
                    }
                }
                else {
                    int power = otherDecay - decay;
                    vec = vec.addVector(facing.getFrontOffsetX() * power, 0, facing.getFrontOffsetZ() * power);
                }
            }
        }

        if(ASMHooks.hasVerticalFlow(world, pos, densityDir, this) == this) {
            for(EnumFacing facing : EnumFacing.Plane.HORIZONTAL) {
                BlockPos offset = pos.offset(facing);
                IBlockState state = world.getBlockState(offset);

                if(canSideFlow(state, world, pos, facing)) {
                    if(causesDownwardCurrent(world, offset, facing) || causesDownwardCurrent(world, offset.down(densityDir), facing)) {
                        vec = vec.normalize().addVector(0.0, 6.0 * densityDir, 0.0);
                        break;
                    }
                }
            }
        }

        return vec.normalize();
    }

    @Override
    public boolean removedByPlayer(IBlockState state, World world, BlockPos pos, EntityPlayer player, boolean willHarvest) {
        onBlockHarvested(world, pos, state, player);
        final Block toCreate = fluid.getBlock() instanceof BlockLiquid ? BlockLiquid.getFlowingBlock(
                fluid.getBlock().getDefaultState().getMaterial()) : fluid.getBlock();

        return world.setBlockState(pos, toCreate.getDefaultState(), world.isRemote ? 11 : 3);
    }

    //================================================================
    //METHODS BELOW THIS EXIST TO MIMIC PROPERTIES OF THE PARENT FLUID
    //================================================================

    @SideOnly(Side.CLIENT)
    @Override
    public int getPackedLightmapCoords(IBlockState state, IBlockAccess world, BlockPos pos) {
        if(fluid.getBlock().getBlockLayer() == MinecraftForgeClient.getRenderLayer())
            if(!(fluid.getBlock() instanceof BlockFluidBase)) return super.getPackedLightmapCoords(state, world, pos);
            else return fluid.getBlock().getDefaultState().getPackedLightmapCoords(world, pos);
        else return world.getCombinedLight(pos, state.getLightValue(world, pos));
    }

    @Override
    public float getFilledPercentage(IBlockAccess world, BlockPos pos) {
        if(!(fluid.getBlock() instanceof BlockFluidBase)) return super.getFilledPercentage(world, pos);
        else return ((BlockFluidBase)fluid.getBlock()).getFilledPercentage(world, pos);
    }

    @Override
    public void onBlockAdded(World world, BlockPos pos, IBlockState state) {
        world.scheduleUpdate(pos, this, fluid.getBlock().tickRate(world));
    }

    @Override
    public void neighborChanged(IBlockState state, World world, BlockPos pos, Block neighborBlock, BlockPos neighbourPos) {
        world.scheduleUpdate(pos, this, fluid.getBlock().tickRate(world));
    }

    @Override
    public int tickRate(World world) {
        return fluid.getBlock().tickRate(world);
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

    @SuppressWarnings("deprecation")
    @Override
    public int getWeakPower(IBlockState blockState, IBlockAccess blockAccess, BlockPos pos, EnumFacing side) {
        return fluid.getBlock().getDefaultState().getWeakPower(blockAccess, pos, side);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int getStrongPower(IBlockState blockState, IBlockAccess blockAccess, BlockPos pos, EnumFacing side) {
        return fluid.getBlock().getDefaultState().getStrongPower(blockAccess, pos, side);
    }

    @Override
    public int getFireSpreadSpeed(IBlockAccess world, BlockPos pos, EnumFacing face) {
        return fluid.getBlock().getFireSpreadSpeed(world, pos, face);
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

    @Override
    public boolean isPassable(IBlockAccess world, BlockPos pos) {
        return fluid.getBlock().isPassable(world, pos);
    }
}
