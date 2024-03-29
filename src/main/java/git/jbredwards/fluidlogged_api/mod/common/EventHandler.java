package git.jbredwards.fluidlogged_api.mod.common;

import git.jbredwards.fluidlogged_api.api.event.FluidloggableEvent;
import git.jbredwards.fluidlogged_api.api.event.FluidloggedEvent;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigHandler;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeModContainer;
import net.minecraftforge.event.entity.player.FillBucketEvent;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.fml.client.event.ConfigChangedEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.Event;
import net.minecraftforge.fml.common.eventhandler.EventPriority;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.items.ItemHandlerHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
@Mod.EventBusSubscriber(modid = FluidloggedAPI.MODID)
public final class EventHandler
{
    @SubscribeEvent(priority = EventPriority.LOWEST, receiveCanceled = true)
    static void forceForgeCascadingFix(@Nonnull ConfigChangedEvent.PostConfigChangedEvent event) {
        if(event.getModID().equals("forge")) ForgeModContainer.fixVanillaCascading = true;
    }

    @SubscribeEvent(priority = EventPriority.HIGH)
    static void handleConfigOverrides(@Nonnull FluidloggableEvent event) {
        final Event.Result result = FluidloggedAPIConfigHandler.isStateFluidloggable(event.state, event.fluid);
        if(result != Event.Result.DEFAULT) {
            event.setCanceled(true);
            event.setResult(result);
        }
    }

    @SubscribeEvent(priority = EventPriority.LOWEST)
    static void updateStaticLiquids(@Nonnull FluidloggedEvent event) {
        if(!event.world.isRemote) {
            //update newly created FluidState
            if(!event.fluidState.isEmpty()) {
                final Block block = event.fluidState.getBlock();
                event.world.scheduleUpdate(event.pos, block, block.tickRate(event.world));
            }
            //update newly created fluid IBlockState (vanilla only)
            else if(event.here.getBlock() instanceof BlockLiquid) {
                final BlockLiquid block = BlockLiquid.getFlowingBlock(event.here.getMaterial());
                event.world.setBlockState(event.pos, block.getDefaultState().withProperty(BlockLiquid.LEVEL, event.here.getValue(BlockLiquid.LEVEL)), 2);
                event.world.scheduleUpdate(event.pos, block, block.tickRate(event.world));
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.NORMAL)
    static void bucketFluidlogging(@Nonnull FillBucketEvent event) {
        if(event.getResult() != Event.Result.DEFAULT) return;
        final EntityPlayer player = event.getEntityPlayer();

        //this does its own raytrace, because the one that comes with the event doesn't do what's need here
        final Vec3d playerVec = new Vec3d(player.posX, player.posY + player.getEyeHeight(), player.posZ);
        final @Nullable RayTraceResult trace = event.getWorld().rayTraceBlocks(playerVec, playerVec.add(
                player.getLookVec().scale(player.getEntityAttribute(EntityPlayer.REACH_DISTANCE).getAttributeValue())));
        if(trace == null) return;

        //don't try fluidlogging without a bucket
        final @Nullable IFluidHandlerItem handler = FluidUtil.getFluidHandler(
                ItemHandlerHelper.copyStackWithSize(event.getEmptyBucket(), 1));
        if(handler == null) return;

        final @Nullable FluidStack contained = handler.drain(Fluid.BUCKET_VOLUME, false);
        if(contained != null && !contained.getFluid().canBePlacedInWorld()) return;

        final BlockPos pos = trace.getBlockPos();
        final World world = event.getWorld();

        //if bucket is empty, try taking from the fluidlogged block
        if(contained == null) {
            if(tryBucketFill(world, pos, player, handler) || tryBucketFill(world, pos.offset(trace.sideHit), player, handler)) {
                event.setFilledBucket(handler.getContainer());
                event.setResult(Event.Result.ALLOW);
            }
        }

        //if bucket has a fluid, try fluidlogging it
        else if(!player.isSneaking() && FluidloggedUtils.isFluidloggableFluid(contained.getFluid().getBlock())) {
            if(tryBucketDrain(world, pos, player, handler, contained) || tryBucketDrain(world, pos.offset(trace.sideHit), player, handler, contained)) {
                event.setFilledBucket(handler.getContainer());
                event.setResult(Event.Result.ALLOW);
            }
        }
    }

    static boolean tryBucketFill(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EntityPlayer player, @Nonnull IFluidHandlerItem handler) {
        final FluidState fluidState = FluidState.get(world, pos);
        if(fluidState.isValid()) {
            if(world.isRemote) return true; //prevent desync
            else if(handler.fill(fluidState.getFluidBlock().drain(world, pos, true), true) == Fluid.BUCKET_VOLUME) {
                world.playSound(null, player.posX, player.posY + 0.5, player.posZ, fluidState.getFluid().getFillSound(world, pos), SoundCategory.BLOCKS, 1, 1);
                return true;
            }
        }

        return false;
    }

    static boolean tryBucketDrain(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EntityPlayer player, @Nonnull IFluidHandlerItem handler, @Nonnull FluidStack stack) {
        final Fluid fluid = stack.getFluid();
        if(FluidloggedUtils.isStateFluidloggable(world.getBlockState(pos), world, pos, fluid)) {
            final FluidState fluidState = FluidState.of(fluid);
            if(fluidState.isValid()) {
                if(world.isRemote) return true; //prevent desync
                else if(handler.drain(new FluidStack(stack, fluidState.getFluidBlock().place(world, pos, stack.copy(), true)), true) != null) {
                    if(!world.provider.doesWaterVaporize() || !fluid.doesVaporize(stack))
                        world.playSound(null, player.posX, player.posY + 0.5, player.posZ, fluid.getEmptySound(stack), SoundCategory.BLOCKS, 1, 1);

                    return true;
                }
            }
        }

        return false;
    }
}
