package git.jbredwards.fluidlogged_api.common.block;

import com.google.common.collect.ImmutableList;
import git.jbredwards.fluidlogged_api.common.event.FluidloggedEvents;
import git.jbredwards.fluidlogged_api.util.FluidloggedUtils;
import mcp.MethodsReturnNonnullByDefault;
import net.minecraft.advancements.CriteriaTriggers;
import net.minecraft.block.*;
import net.minecraft.block.material.EnumPushReaction;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.particle.ParticleDigging;
import net.minecraft.client.particle.ParticleManager;
import net.minecraft.enchantment.EnchantmentHelper;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.init.Blocks;
import net.minecraft.init.Enchantments;
import net.minecraft.item.Item;
import net.minecraft.item.ItemSlab;
import net.minecraft.item.ItemStack;
import net.minecraft.pathfinding.PathNodeType;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.*;
import net.minecraft.util.math.*;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldServer;
import net.minecraftforge.common.IShearable;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.ParametersAreNonnullByDefault;
import java.util.List;
import java.util.Objects;
import java.util.Random;

/**
 * this is the block responsible for storing fluidlogged states in the world.
 * new instances of this block should not be created externally, use IFluidloggable instead.
 * if you need more functionality from IFluidloggable, create an issue or pull request on git.
 * @author jbred
 *
 */
@MethodsReturnNonnullByDefault
@ParametersAreNonnullByDefault
public class BlockFluidloggedTE extends AbstractFluidloggedBlock implements ITileEntityProvider, IGrowable, IShearable
{
    public BlockFluidloggedTE(Fluid fluid, Material material, MapColor mapColor) {
        super(fluid, material, mapColor);
    }

    public BlockFluidloggedTE(Fluid fluid, Material material) {
        super(fluid, material);
    }

    //returns barriers if null (should never be null)
    @Nonnull
    public IBlockState getStored(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final @Nullable TileEntity te = world.getTileEntity(pos);

        if(te instanceof TileEntityFluidlogged) return ((TileEntityFluidlogged)te).stored;
        //should never pass
        return Blocks.BARRIER.getDefaultState();
    }

    public void setStored(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState stored, boolean notify) {
        final @Nullable TileEntity te = world.getTileEntity(pos);

        //if the block here is a fluidlogged one
        if(te instanceof TileEntityFluidlogged) {
            //if the block can be fluidlogged
            if(FluidloggedUtils.isStateFluidloggable(stored, null)) ((TileEntityFluidlogged)te).setStored(stored, notify);
            //if the block can't be fluidlogged
            else if(world instanceof World) {
                //if the block is not air
                if(stored.getMaterial() != Material.AIR) ((World)world).setBlockState(pos, stored);
                //if the block is air
                else ((World)world).setBlockState(pos, fluid.getBlock().getDefaultState());
            }
        }
    }

    @Override
    public String getUnlocalizedName() {
        return fluid.getBlock().getUnlocalizedName();
    }

    @Override
    public String getLocalizedName() {
        return fluid.getBlock().getLocalizedName();
    }

    @SuppressWarnings("deprecation")
    @Override
    public boolean eventReceived(IBlockState state, World worldIn, BlockPos pos, int id, int param) { return id == 1; }

    @Nullable
    @Override
    public TileEntityFluidlogged createNewTileEntity(World worldIn, int meta) {
        return new TileEntityFluidlogged();
    }

    @Nullable
    @Override
    public TileEntityFluidlogged createTileEntity(World world, IBlockState state) {
        return new TileEntityFluidlogged();
    }

    @SuppressWarnings("deprecation")
    @Override
    public EnumPushReaction getMobilityFlag(IBlockState state) { return EnumPushReaction.NORMAL; }

    @Nullable
    @Override
    public FluidStack drain(World world, BlockPos pos, boolean doDrain) {
        if(doDrain && !FluidloggedUtils.tryUnfluidlogBlock(world, pos, null, null)) return null;
        else return stack.copy();
    }

    @SideOnly(Side.CLIENT)
    @Override
    public boolean shouldSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState stored = getStored(world, pos);
        final IBlockState offset = world.getBlockState(pos.offset(side));

        if(stored.getRenderType() == EnumBlockRenderType.INVISIBLE && FluidloggedUtils.getFluidFromBlock(offset.getBlock()) == fluid) {
            return !FluidloggedUtils.isSource(world, pos.offset(side), offset);
        }
        else if(stored.shouldSideBeRendered(world, pos, side)) return super.shouldSideBeRendered(state, world, pos, side);

        //default
        return fluid.getBlock().getDefaultState().shouldSideBeRendered(world, pos, side);
    }

    @Override
    public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
        super.updateTick(world, pos, state, rand);
        final boolean canFlowVertical = canSideFlow(state, world, pos, densityDir < 0 ? EnumFacing.DOWN : EnumFacing.UP);
        //Fluidlogs nearby blocks if enough sources
        if(canCreateSources && (!canFlowVertical || !canFlowInto(world, pos.up(densityDir)) || FluidloggedEvents.getFluidFromBlockSource(world.getBlockState(pos.up(densityDir))) == fluid)) {
            for(EnumFacing parentFacing : EnumFacing.HORIZONTALS) {
                BlockPos parentOffset = pos.offset(parentFacing);

                if(world.isBlockLoaded(parentOffset) && canSideFlow(state, world, pos, parentFacing)) {
                    IBlockState parentState = world.getBlockState(parentOffset);

                    if(parentState.getBlockFaceShape(world, parentOffset, parentFacing.getOpposite()) != BlockFaceShape.SOLID && FluidloggedUtils.isStateFluidloggable(parentState, fluid)) {
                        for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                            BlockPos offset = parentOffset.offset(facing);
                            //doesn't repeat the check for the original fluid source & that the other fluid source can flow into the side
                            if(world.isBlockLoaded(offset) && facing.getOpposite() != parentFacing && parentState.getBlockFaceShape(world, offset, facing) != BlockFaceShape.SOLID) {
                                IBlockState blockState = world.getBlockState(offset);

                                if(blockState.getBlock() instanceof BlockFluidloggedTE && FluidloggedUtils.getFluidFromBlock(blockState.getBlock()) == fluid) {
                                    BlockFluidloggedTE block = (BlockFluidloggedTE) blockState.getBlock();
                                    if(block.canSideFlow(blockState, world, offset, facing.getOpposite())) {
                                        boolean vertical = !block.canSideFlow(blockState, world, offset, densityDir < 0 ? EnumFacing.DOWN : EnumFacing.UP) || !block.canFlowInto(world, offset.up(densityDir));
                                        if(vertical || FluidloggedEvents.getFluidFromBlockSource(world.getBlockState(offset.up(densityDir))) == fluid) {
                                            FluidloggedUtils.tryFluidlogBlock(world, parentOffset, parentState, fluid, true, true);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    //================================================
    //BELOW METHODS ARE USED TO MIMIC THE STORED BLOCK
    //================================================

    @Override
    public boolean canSideFlow(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing side) {
        final IBlockState stored = getStored(world, pos);

        //fixes trapdoors
        if(stored.getBlock() instanceof BlockTrapDoor) {
            if(!stored.getValue(BlockTrapDoor.OPEN)) {
                final boolean bottom = stored.getValue(BlockTrapDoor.HALF) == BlockTrapDoor.DoorHalf.BOTTOM;

                if(bottom && side == EnumFacing.DOWN) return false;
                else return bottom || side != EnumFacing.UP;
            }
            else return stored.getValue(BlockTrapDoor.FACING).getOpposite() != side;
        }

        //default
        if(stored.getBlock() instanceof IFluidloggable) return ((IFluidloggable)stored.getBlock()).canSideFlow(stored, fluid, world, pos, side);
        else return stored.getBlockFaceShape(world, pos, side) != BlockFaceShape.SOLID;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public boolean shouldFluidRender(IBlockState state, IBlockAccess world, BlockPos pos) {
        final IBlockState stored = getStored(world, pos);
        return !(stored.getBlock() instanceof IFluidloggable) || ((IFluidloggable)stored.getBlock()).shouldFluidRender(stored, fluid, world, pos);
    }

    @SuppressWarnings("deprecation")
    @Override
    public float getPlayerRelativeBlockHardness(IBlockState state, EntityPlayer player, World worldIn, BlockPos pos) {
        final IBlockState stored = getStored(worldIn, pos);
        final float hardness = stored.getBlockHardness(worldIn, pos);

        if(hardness < 0) return 0;
        else if(canHarvestBlock(stored, player, worldIn, pos)) return player.getDigSpeed(stored, pos) / hardness / 30;
        else return player.getDigSpeed(stored, pos) / hardness / 100;
    }

    public boolean canHarvestBlock(IBlockState state, EntityPlayer player, IBlockAccess world, BlockPos pos) {
        state = state.getActualState(world, pos);
        if(state.getMaterial().isToolNotRequired()) return true;

        //tool type
        final ItemStack held = player.getHeldItemMainhand();
        @Nullable String tool = state.getBlock().getHarvestTool(state);
        tool = (tool == null && state.getMaterial() == Material.ROCK ? "pickaxe" : tool);

        //if no tool is required
        if(held.isEmpty() || tool == null) return player.canHarvestBlock(state);

        //tool level
        final int level = held.getItem().getHarvestLevel(held, tool, player, state);

        //if the tool level is sufficient
        if(level > 0) return player.canHarvestBlock(state);
        else return level >= state.getBlock().getHarvestLevel(state);
    }

    @Override
    public boolean addLandingEffects(IBlockState state, WorldServer worldObj, BlockPos blockPosition, IBlockState iblockstate, EntityLivingBase entity, int numberOfParticles) {
        state = getStored(worldObj, blockPosition);

        if(!state.getBlock().addLandingEffects(state, worldObj, blockPosition, iblockstate, entity, numberOfParticles)) {
            worldObj.spawnParticle(EnumParticleTypes.BLOCK_DUST, entity.posX, entity.posY, entity.posZ, numberOfParticles, 0, 0, 0, 0.15000000596046448, getStateId(state));
        }

        return true;
    }

    @Override
    public boolean addRunningEffects(IBlockState state, World world, BlockPos pos, Entity entity) {
        state = getStored(world, pos);

        if(state.getRenderType() != EnumBlockRenderType.INVISIBLE && !state.getBlock().addRunningEffects(state, world, pos, entity)) {
            world.spawnParticle(EnumParticleTypes.BLOCK_CRACK, entity.posX + new Random().nextFloat() - 0.5 * entity.width, entity.getEntityBoundingBox().minY + 0.1, entity.posZ + new Random().nextFloat() - 0.5 * entity.width, -entity.motionX * 4, 1.5, -entity.motionZ * 4, getStateId(state));
        }

        return true;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public boolean addHitEffects(IBlockState state, World worldObj, RayTraceResult target, ParticleManager manager) {
        final BlockPos pos = target.getBlockPos();
        final EnumFacing side = target.sideHit;
        state = getStored(worldObj, pos);

        if(!state.getBlock().addHitEffects(state, worldObj, target, manager) && state.getRenderType() != EnumBlockRenderType.INVISIBLE) {
            final AxisAlignedBB aabb = state.getBoundingBox(worldObj, pos);
            double x = pos.getX() + worldObj.rand.nextDouble() * (aabb.maxX - aabb.minX - 0.20000000298023224) + 0.10000000149011612 + aabb.minX;
            double y = pos.getY() + worldObj.rand.nextDouble() * (aabb.maxY - aabb.minY - 0.20000000298023224) + 0.10000000149011612 + aabb.minY;
            double z = pos.getZ() + worldObj.rand.nextDouble() * (aabb.maxZ - aabb.minZ - 0.20000000298023224) + 0.10000000149011612 + aabb.minZ;

            if(side == EnumFacing.DOWN) y = pos.getY() + aabb.minY - 0.10000000149011612;
            if(side == EnumFacing.UP) y = pos.getY() + aabb.maxY + 0.10000000149011612;
            if(side == EnumFacing.NORTH) z = pos.getZ() + aabb.minZ - 0.10000000149011612;
            if(side == EnumFacing.SOUTH) z = pos.getZ() + aabb.maxZ + 0.10000000149011612;
            if(side == EnumFacing.WEST) x = pos.getX() + aabb.minX - 0.10000000149011612;
            if(side == EnumFacing.EAST) x = pos.getX() + aabb.maxX + 0.10000000149011612;

            manager.addEffect(((ParticleDigging)(Objects.requireNonNull(new ParticleDigging.Factory().createParticle(0, worldObj, x, y, z, 0, 0, 0, getStateId(state))))).setBlockPos(pos).multiplyVelocity(0.2f).multipleParticleScaleBy(0.6f));
        }

        return true;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public boolean addDestroyEffects(World world, BlockPos pos, ParticleManager manager) {
        final IBlockState stored = getStored(world, pos);

        if(!stored.getBlock().addDestroyEffects(world, pos, manager)) {
            for(int x = 0; x < 4; ++x) {
                for(int y = 0; y < 4; ++y) {
                    for(int z = 0; z < 4; ++z) {
                        double posX = (x + 0.5) / 4;
                        double posY = (y + 0.5) / 4;
                        double posZ = (z + 0.5) / 4;
                        manager.addEffect(((ParticleDigging)(Objects.requireNonNull(new ParticleDigging.Factory().createParticle(0, world, pos.getX() + posX, pos.getY() + posY, pos.getZ() + posZ, posX - 0.5, posY - 0.5, posZ - 0.5, getStateId(stored))))).setBlockPos(pos));
                    }
                }
            }
        }

        return true;
    }

    @SuppressWarnings("deprecation")
    @Override
    public boolean removedByPlayer(IBlockState state, World world, BlockPos pos, EntityPlayer player, boolean willHarvest) {
        final IBlockState stored = getStored(world, pos);

        if(!player.isCreative() && canHarvestBlock(stored, player, world, pos)) {
            final int fortune = EnchantmentHelper.getEnchantmentLevel(Enchantments.FORTUNE, player.getHeldItemMainhand());
            final List<ItemStack> drops = stored.getBlock().getDrops(world, pos, stored, fortune);

            for(ItemStack drop : drops) {
                spawnAsEntity(world, pos, drop);
            }
        }

        return world.setBlockState(pos, fluid.getBlock().getDefaultState(), world.isRemote ? 11 : 3);
    }

    //wall & fence connection fixes
    @Override
    public boolean canBeConnectedTo(IBlockAccess world, BlockPos pos, EnumFacing facing) {
        final IBlockState stored = getStored(world, pos);

        //wall
        if(stored.getBlock() instanceof BlockWall) {
            final BlockFaceShape shape = stored.getBlockFaceShape(world, pos, facing);
            return (shape == BlockFaceShape.MIDDLE_POLE_THICK || shape == BlockFaceShape.MIDDLE_POLE) && stored.getBlock() instanceof BlockFenceGate;
        }
        //fence
        else if(stored.getBlock() instanceof BlockFence) {
            final BlockFaceShape shape = stored.getBlockFaceShape(world, pos, facing);
            final IBlockState neighbor = FluidloggedUtils.getStoredOrReal(world, pos.offset(facing));

            return shape == BlockFaceShape.MIDDLE_POLE && stored.getMaterial() == neighbor.getMaterial();

        }
        //default
        else return stored.getBlock().canBeConnectedTo(world, pos, facing);
    }

    //slab functionality
    @Override
    public boolean onBlockActivated(World worldIn, BlockPos pos, IBlockState state, EntityPlayer playerIn, EnumHand hand, EnumFacing facing, float hitX, float hitY, float hitZ) {
        final IBlockState stored = getStored(worldIn, pos);
        final ItemStack held = playerIn.getHeldItem(hand);
        final boolean storedAction = stored.getBlock().onBlockActivated(worldIn, pos, stored, playerIn, hand, facing, hitX, hitY, hitZ);

        //checks if the player is trying to create a double slab
        if(!storedAction && stored.getBlock() instanceof BlockSlab && held.getItem() instanceof ItemSlab && ((ItemSlab)held.getItem()).getBlock() == stored.getBlock()) {
            final ItemSlab itemSlab = (ItemSlab)held.getItem();
            final BlockSlab blockSlab = (BlockSlab)stored.getBlock();
            final boolean topHalf = stored.getValue(BlockSlab.HALF) == BlockSlab.EnumBlockHalf.TOP;

            if((topHalf && facing == EnumFacing.DOWN) || (!topHalf && facing == EnumFacing.UP)) {
                final IBlockState doubleSlab = itemSlab.makeState(blockSlab.getVariantProperty(), stored.getValue(blockSlab.getVariantProperty()));
                final AxisAlignedBB slabAABB = doubleSlab.getCollisionBoundingBox(worldIn, pos);

                //does the place
                if((slabAABB == null || worldIn.checkNoEntityCollision(slabAABB.offset(pos))) && worldIn.setBlockState(pos, doubleSlab, 11)) {
                    final SoundType sound = doubleSlab.getBlock().getSoundType(doubleSlab, worldIn, pos, playerIn);
                    worldIn.playSound(null, pos, sound.getPlaceSound(), SoundCategory.BLOCKS, (sound.getVolume() + 1) / 2, sound.getPitch() * 0.8f);

                    if(playerIn instanceof EntityPlayerMP) CriteriaTriggers.PLACED_BLOCK.trigger((EntityPlayerMP)playerIn, pos, held);
                    if(!playerIn.isCreative()) held.shrink(1);

                    //updates this just in case the stored block changes
                    worldIn.scheduleUpdate(pos, this, tickRate);

                    return true;
                }
            }
        }

        //default
        return storedAction;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public void randomDisplayTick(IBlockState stateIn, World worldIn, BlockPos pos, Random rand) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().randomDisplayTick(stored, worldIn, pos, rand);

        super.randomDisplayTick(stateIn, worldIn, pos, rand);

        //show fluidlogged barrier when item is held
        final EntityPlayer player = Minecraft.getMinecraft().player;
        if(stored.getBlock() == Blocks.BARRIER && player.isCreative() && player.getHeldItemMainhand().getItem() == Item.getItemFromBlock(Blocks.BARRIER)) {
            worldIn.spawnParticle(EnumParticleTypes.BARRIER, pos.getX() + 0.5, pos.getY() + 0.5, pos.getZ() + 0.5, 0, 0, 0);
        }
    }

    @Override
    public void onBlockClicked(World worldIn, BlockPos pos, EntityPlayer playerIn) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().onBlockClicked(worldIn, pos, playerIn);
    }

    @Override
    public void onEntityWalk(World worldIn, BlockPos pos, Entity entityIn) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().onEntityWalk(worldIn, pos, entityIn);

        super.onEntityWalk(worldIn, pos, entityIn);
    }

    @Override
    public void onEntityCollidedWithBlock(World worldIn, BlockPos pos, IBlockState state, Entity entityIn) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().onEntityCollidedWithBlock(worldIn, pos, stored, entityIn);

        super.onEntityCollidedWithBlock(worldIn, pos, state, entityIn);
    }

    @Override
    public boolean canHarvestBlock(IBlockAccess world, BlockPos pos, EntityPlayer player) {
        return canHarvestBlock(getStored(world, pos), player, world, pos);
    }

    @Override
    public void breakBlock(World worldIn, BlockPos pos, IBlockState state) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().breakBlock(worldIn, pos, stored);
    }

    @Override
    public boolean rotateBlock(World world, BlockPos pos, EnumFacing axis) {
        return getStored(world, pos).getBlock().rotateBlock(world, pos, axis);
    }

    @Nonnull
    @Override
    public Vec3d modifyAcceleration(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Entity entity, @Nonnull Vec3d vec) {
        final Vec3d oldSpeed = super.modifyAcceleration(world, pos, entity, vec);
        final Vec3d newSpeed = getStored(world, pos).getBlock().modifyAcceleration(world, pos, entity, vec);

        return oldSpeed.add(newSpeed);
    }

    @Override
    public void neighborChanged(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Block neighborBlock, @Nonnull BlockPos neighbourPos) {
        final IBlockState stored = getStored(world, pos);
        stored.neighborChanged(world, pos, neighborBlock, neighbourPos);

        super.neighborChanged(state, world, pos, neighborBlock, neighbourPos);
    }

    @Override
    public void randomTick(World worldIn, BlockPos pos, IBlockState state, Random random) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().randomTick(worldIn, pos, stored, random);
    }

    @Nullable
    @Override
    public PathNodeType getAiPathNodeType(IBlockState state, IBlockAccess world, BlockPos pos, @Nullable EntityLiving entity) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock().getAiPathNodeType(stored, world, pos, entity);
    }

    @Override
    public void onFallenUpon(World worldIn, BlockPos pos, Entity entityIn, float fallDistance) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().onFallenUpon(worldIn, pos, entityIn, fallDistance);
    }

    @SuppressWarnings("deprecation")
    @Nullable
    @Override
    public PathNodeType getAiPathNodeType(IBlockState state, IBlockAccess world, BlockPos pos) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock().getAiPathNodeType(stored, world, pos);
    }

    @SuppressWarnings("deprecation")
    @Override
    public AxisAlignedBB getBoundingBox(IBlockState state, IBlockAccess source, BlockPos pos) {
        final IBlockState stored = getStored(source, pos);
        return stored.getBoundingBox(source, pos);
    }

    @Override
    public AxisAlignedBB getCollisionBoundingBox(@Nonnull IBlockState blockState, @Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) {
        final IBlockState stored = getStored(worldIn, pos);
        return stored.getCollisionBoundingBox(worldIn, pos);
    }

    @SuppressWarnings("deprecation")
    @SideOnly(Side.CLIENT)
    @Override
    public AxisAlignedBB getSelectedBoundingBox(IBlockState state, World worldIn, BlockPos pos) {
        final IBlockState stored = getStored(worldIn, pos);
        return stored.getSelectedBoundingBox(worldIn, pos);
    }

    @Override
    public void harvestBlock(World worldIn, EntityPlayer player, BlockPos pos, IBlockState state, @Nullable TileEntity te, ItemStack stack) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().harvestBlock(worldIn, player, pos, stored, te, stack);
    }

    @Override
    public void onBlockHarvested(World worldIn, BlockPos pos, IBlockState state, EntityPlayer player) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.getBlock().onBlockHarvested(worldIn, pos, stored, player);
    }

    @Override
    public int getExpDrop(IBlockState state, IBlockAccess world, BlockPos pos, int fortune) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock().getExpDrop(stored, world, pos, fortune);
    }

    @SuppressWarnings("deprecation")
    @Override
    public ItemStack getItem(World worldIn, BlockPos pos, IBlockState state) {
        final IBlockState stored = getStored(worldIn, pos);
        return stored.getBlock().getItem(worldIn, pos, stored);
    }

    @Override
    public ItemStack getPickBlock(IBlockState state, RayTraceResult target, World world, BlockPos pos, EntityPlayer player) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock().getPickBlock(stored, target, world, pos, player);
    }

    @Override
    public boolean canSilkHarvest(World world, BlockPos pos, IBlockState state, EntityPlayer player) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock().canSilkHarvest(world, pos, stored, player);
    }

    @Override
    public SoundType getSoundType(IBlockState state, World world, BlockPos pos, @Nullable Entity entity) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock().getSoundType(stored, world, pos, entity);
    }

    @Override
    public float getExplosionResistance(World world, BlockPos pos, @Nullable Entity exploder, Explosion explosion) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock().getExplosionResistance(world, pos, exploder, explosion);
    }

    @SuppressWarnings("deprecation")
    @Override
    public float getBlockHardness(IBlockState blockState, World worldIn, BlockPos pos) {
        final IBlockState stored = getStored(worldIn, pos);
        return stored.getBlockHardness(worldIn, pos);
    }

    @SuppressWarnings("deprecation")
    @Nullable
    @Override
    public RayTraceResult collisionRayTrace(IBlockState blockState, World worldIn, BlockPos pos, Vec3d start, Vec3d end) {
        final IBlockState stored = getStored(worldIn, pos);
        return stored.collisionRayTrace(worldIn, pos, start, end);
    }

    @SuppressWarnings("deprecation")
    @Override
    public void addCollisionBoxToList(IBlockState state, World worldIn, BlockPos pos, AxisAlignedBB entityBox, List<AxisAlignedBB> collidingBoxes, @Nullable Entity entityIn, boolean isActualState) {
        final IBlockState stored = getStored(worldIn, pos);
        stored.addCollisionBoxToList(worldIn, pos, entityBox, collidingBoxes, entityIn, isActualState);
    }

    @Nonnull
    @Override
    public BlockFaceShape getBlockFaceShape(@Nonnull IBlockAccess worldIn, @Nonnull IBlockState state, @Nonnull BlockPos pos, @Nonnull EnumFacing face) {
        final IBlockState stored = getStored(worldIn, pos);
        return stored.getBlockFaceShape(worldIn, pos, face);
    }

    @Override
    public int getLightValue(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState stored = getStored(world, pos);
        return Math.min(stored.getLightValue(world, pos) + super.getLightValue(state, world, pos), 15);
    }

    @Override
    public int getWeakPower(IBlockState blockState, IBlockAccess blockAccess, BlockPos pos, EnumFacing side) {
        return Math.min(super.getWeakPower(blockState, blockAccess, pos, side) + getStored(blockAccess, pos).getWeakPower(blockAccess, pos, side), 15);
    }

    @Override
    public int getStrongPower(IBlockState blockState, IBlockAccess blockAccess, BlockPos pos, EnumFacing side) {
        return Math.min(super.getStrongPower(blockState, blockAccess, pos, side) + getStored(blockAccess, pos).getStrongPower(blockAccess, pos, side), 15);
    }

    @Override
    public boolean canGrow(World worldIn, BlockPos pos, IBlockState state, boolean isClient) {
        final IBlockState stored = getStored(worldIn, pos);
        return stored.getBlock() instanceof IGrowable && ((IGrowable)stored.getBlock()).canGrow(worldIn, pos, stored, isClient);
    }

    @Override
    public boolean canUseBonemeal(World worldIn, Random rand, BlockPos pos, IBlockState state) {
        final IBlockState stored = getStored(worldIn, pos);
        return stored.getBlock() instanceof IGrowable && ((IGrowable)stored.getBlock()).canUseBonemeal(worldIn, rand, pos, stored);
    }

    @Override
    public void grow(World worldIn, Random rand, BlockPos pos, IBlockState state) {
        final IBlockState stored = getStored(worldIn, pos);
        if(stored.getBlock() instanceof IGrowable) ((IGrowable)stored.getBlock()).grow(worldIn, rand, pos, stored);
    }

    @Override
    public boolean isShearable(@Nonnull ItemStack item, IBlockAccess world, BlockPos pos) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock() instanceof IShearable && ((IShearable)stored.getBlock()).isShearable(item, world, pos);
    }

    @Nonnull
    @Override
    public List<ItemStack> onSheared(@Nonnull ItemStack item, IBlockAccess world, BlockPos pos, int fortune) {
        final IBlockState stored = getStored(world, pos);
        return stored.getBlock() instanceof IShearable ? ((IShearable)stored.getBlock()).onSheared(item, world, pos, fortune) : ImmutableList.of();
    }
}
