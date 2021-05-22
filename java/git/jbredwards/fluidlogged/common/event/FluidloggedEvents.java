package git.jbredwards.fluidlogged.common.event;

import git.jbredwards.fluidlogged.Fluidlogged;
import git.jbredwards.fluidlogged.common.block.AbstractFluidloggedBlock;
import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.common.block.IFluidloggable;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import git.jbredwards.fluidlogged.util.FluidloggedConstants;
import git.jbredwards.fluidlogged.util.FluidloggedUtils;
import net.minecraft.advancements.CriteriaTriggers;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.SoundType;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.block.statemap.StateMap;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.init.Items;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.util.*;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.model.ModelLoader;
import net.minecraftforge.common.ForgeModContainer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.event.entity.player.FillBucketEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.fml.common.eventhandler.Event;
import net.minecraftforge.fml.common.eventhandler.EventPriority;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.registry.GameRegistry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nullable;

import java.util.Optional;

import static net.minecraftforge.fluids.BlockFluidBase.LEVEL;

/**
 *
 * @author jbred
 *
 */
public final class FluidloggedEvents
{
    //registers the water & lava fluidlogged te's
    @SuppressWarnings("unused")
    @SubscribeEvent
    public void registerBlocks(RegistryEvent.Register<Block> event) {
        event.getRegistry().registerAll(Fluidlogged.WATERLOGGED_TE, Fluidlogged.LAVALOGGED_TE);
        GameRegistry.registerTileEntity(TileEntityFluidlogged.class, new ResourceLocation(FluidloggedConstants.MODID, "te"));
    }

    //registers the water & lava fluidlogged te custom state mappers
    @SuppressWarnings("unused")
    @SubscribeEvent
    public void registerModels(ModelRegistryEvent event) {
        ModelLoader.setCustomStateMapper(Fluidlogged.WATERLOGGED_TE, new StateMap.Builder().ignore(LEVEL).build());
        ModelLoader.setCustomStateMapper(Fluidlogged.LAVALOGGED_TE, new StateMap.Builder().ignore(LEVEL).build());
    }

    //===================
    //FLUIDLOGGING EVENTS
    //===================

    //fired when a player tried to take from a fluidlogged te, or when they try to fluidlog a block
    @SuppressWarnings("unused")
    @SubscribeEvent(priority = EventPriority.HIGH)
    public void fluidPlaceOrTake(FillBucketEvent event) {
        final @Nullable IFluidHandlerItem handler = FluidUtil.getFluidHandler(event.getEmptyBucket());

        //this shouldn't be null, but just in case
        if(handler != null) {
            //null if the bucket is empty
            final @Nullable FluidStack fluidStack = handler.drain(Fluid.BUCKET_VOLUME, false);
            final BlockPos pos = event.getTarget().getBlockPos();
            final IBlockState here = event.getWorld().getBlockState(pos);

            LogManager.getFormatterLogger("fluidlogged").info(here.getBlock().getRegistryName().toString());

            //if the fluidStack is empty, the bucket is empty
            if(fluidStack == null) {
                //if the block here is fluidlogged
                if(here.getBlock() instanceof BlockFluidloggedTE) {
                    actionBucketEmpty(event.getWorld(), here, event.getEmptyBucket(), event.getTarget());
                    event.setFilledBucket(FluidloggedUtils.getFilledBucket(event.getEmptyBucket(), ((BlockFluidloggedTE)here.getBlock()).fluid));
                    event.setResult(Event.Result.ALLOW);
                }
                //if the block here is a normal fluidlogged block, the block should not be drained
                else if(here.getBlock() instanceof AbstractFluidloggedBlock && !here.getBlock().isReplaceable(event.getWorld(), pos)) {
                    event.setResult(Event.Result.DENY);
                    event.setCanceled(true);
                }
                else {
                    final IBlockState offset = event.getWorld().getBlockState(pos.offset(event.getTarget().sideHit));
                    if(offset.getBlock() instanceof AbstractFluidloggedBlock && !offset.getBlock().isReplaceable(event.getWorld(), pos)) {
                        event.setResult(Event.Result.DENY);
                        event.setCanceled(true);
                    }
                }
            }
            //since the bucket if full, the block will get fluidlogged
            else if(FluidloggedUtils.isStateFluidloggable(here, fluidStack.getFluid())) {
                final @Nullable BlockFluidloggedTE block = FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.get(fluidStack.getFluid());

                if(block != null) {
                    actionBucketFull(event.getWorld(), here, event.getEmptyBucket(), event.getTarget(), fluidStack, block);
                    //new UniversalBucket().onFillBucket(event);
                    event.setResult(Event.Result.ALLOW);
                }
            }
            //if the block here should not get replaced with fluid
            else if(here.getBlock() instanceof AbstractFluidloggedBlock && !here.getBlock().isReplaceable(event.getWorld(), pos)) {
                event.setResult(Event.Result.DENY);
                event.setCanceled(true);
            }
            else {
                final IBlockState offset = event.getWorld().getBlockState(pos.offset(event.getTarget().sideHit));
                if(offset.getBlock() instanceof AbstractFluidloggedBlock && !offset.getBlock().isReplaceable(event.getWorld(), pos)) {
                    event.setResult(Event.Result.DENY);
                    event.setCanceled(true);
                }
            }
        }
    }

    private static void actionBucketEmpty(World world, IBlockState here, ItemStack bucket, RayTraceResult trace) {

    }

    private static void actionBucketFull(World world, IBlockState here, ItemStack bucket, RayTraceResult trace, FluidStack fluidStack, BlockFluidloggedTE block) {

    }

    //allows the player to place fluidloggable blocks into fluid and have them become fluidlogged
    @SuppressWarnings("unused")
    @SubscribeEvent
    public void placeBlockInFluid(PlayerInteractEvent.RightClickBlock event) {
        final @Nullable EnumFacing facing = event.getFace();
        final ItemStack held = event.getItemStack();

        //if the held item is a block
        if(facing != null && held.getItem() instanceof ItemBlock) {
            final World world = event.getWorld();
            final BlockPos pos = event.getPos().offset(facing);
            final IBlockState here = world.getBlockState(pos);
            final @Nullable Fluid fluid = getFluid(here);

            //if the block is a fluid
            if(fluid != null && !(here.getBlock() instanceof AbstractFluidloggedBlock)) {
                final int meta = held.getItem().getMetadata(held.getMetadata());
                final Vec3d hit = Optional.ofNullable(event.getHitVec()).orElse(new Vec3d(pos));
                final EntityPlayer player = event.getEntityPlayer();

                IBlockState stored = ((ItemBlock)held.getItem()).getBlock().getStateForPlacement(world, pos, facing, (float)hit.x - pos.getX(), (float)hit.y - pos.getY(), (float)hit.z - pos.getZ(), meta, player, event.getHand());
                if(stored.getBlock() instanceof IFluidloggable) stored = ((IFluidloggable)stored.getBlock()).getFluidloggedState(world, pos, stored);

                //if the held block can be fluidlogged
                if(FluidloggedUtils.isStateFluidloggable(stored, fluid)) {
                    final FluidloggedEvent.Fluidlog fluidlog = new FluidloggedEvent.Fluidlog(world, pos, here, stored, FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.get(fluid), new TileEntityFluidlogged());
                    if(!MinecraftForge.EVENT_BUS.post(fluidlog)) {
                        stored = fluidlog.stored;
                        fluidlog.te.setStored(stored, false);

                        //does the place
                        world.setBlockState(pos, fluidlog.block.getDefaultState());
                        world.setTileEntity(pos, fluidlog.te);

                        //place sound
                        final SoundType sound = stored.getBlock().getSoundType(stored, world, pos, player);
                        world.playSound(null, pos, sound.getPlaceSound(), SoundCategory.BLOCKS, (sound.getVolume() + 1) / 2, sound.getPitch() * 0.8f);

                        //updates the player's stats
                        if(player instanceof EntityPlayerMP) CriteriaTriggers.PLACED_BLOCK.trigger((EntityPlayerMP)player, pos, event.getItemStack());
                        if(!player.isCreative()) event.getItemStack().shrink(1);

                        event.setCanceled(true);
                        event.setCancellationResult(EnumActionResult.SUCCESS);
                    }
                }
            }
        }
    }

    //gets the fluid here, null if none
    @Nullable
    public static Fluid getFluid(IBlockState state) {
        if((state.getBlock() instanceof BlockFluidClassic || state.getBlock() instanceof BlockLiquid) && state.getValue(BlockLiquid.LEVEL) == 0) {
            if(state.getBlock() instanceof BlockFluidClassic) return ((BlockFluidClassic)state.getBlock()).getFluid();
            else {
                if(state.getMaterial() == Material.WATER) return FluidRegistry.WATER;
                else return FluidRegistry.LAVA;
            }
        }

        return null;
    }
}
