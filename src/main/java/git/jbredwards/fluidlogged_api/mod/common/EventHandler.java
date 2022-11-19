package git.jbredwards.fluidlogged_api.mod.common;

import git.jbredwards.fluidlogged_api.api.event.FluidloggableEvent;
import git.jbredwards.fluidlogged_api.api.event.FluidloggedEvent;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.player.FillBucketEvent;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
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
    @SubscribeEvent(priority = EventPriority.HIGH)
    static void handleConfigOverrides(@Nonnull FluidloggableEvent event) {
        final Event.Result result = ConfigHandler.isStateFluidloggable(event.state, event.fluid);
        if(result != Event.Result.DEFAULT) {
            event.setCanceled(true);
            event.setResult(result);
        }
    }

    @SubscribeEvent(priority = EventPriority.LOWEST)
    static void updateStaticLiquids(@Nonnull FluidloggedEvent event) {
        if(!event.doesVaporize() && !event.fluidState.isEmpty()) {
            final Block block = event.fluidState.getBlock();
            if(block instanceof BlockLiquid) event.world.scheduleUpdate(event.pos, block, block.tickRate(event.world));
        }
    }

    @SubscribeEvent(priority = EventPriority.LOW)
    static void destroyBurnables(@Nonnull FluidloggedEvent event) {
        if(ConfigHandler.lavalogVaporizeFlammable && !event.doesVaporize() && !event.fluidState.isEmpty()) {
            //check temperature, fallback on blocks that light entities on fire
            if(event.fluidState.getFluid().getTemperature() >= 1000 || event.fluidState.getMaterial() == Material.LAVA) {
                //check game rule
                if(event.world.getGameRules().getBoolean("doFireTick")) {
                    boolean isFlammable = event.here.getMaterial().getCanBurn();
                    if(!isFlammable) {
                        for(EnumFacing facing : EnumFacing.values()) {
                            if(event.here.getBlock().isFlammable(event.world, event.pos, facing)) {
                                isFlammable = true;
                                break;
                            }
                        }
                    }

                    if(isFlammable) {
                        final IBlockState state = event.fluidState.getBlock() instanceof BlockLiquid
                                ? BlockLiquid.getFlowingBlock(Material.LAVA).getDefaultState()
                                : event.fluidState.getState();

                        event.fluidState.getFluid().vaporize(null, event.world, event.pos, event.getFluidStack());
                        event.world.setBlockState(event.pos, state, event.blockFlags);
                        event.setResult(Event.Result.ALLOW);
                        event.setCanceled(true);
                    }
                }
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
        else if(FluidloggedUtils.isFluidloggableFluid(contained.getFluid().getBlock())) {
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
                world.playSound(null, player.posX, player.posY + 0.5, player.posZ, fluidState.getFluid().getFillSound(), SoundCategory.BLOCKS, 1, 1);
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
