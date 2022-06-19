package git.jbredwards.fluidlogged_api.mod.common;

import git.jbredwards.fluidlogged_api.mod.Constants;
import git.jbredwards.fluidlogged_api.mod.Main;
import git.jbredwards.fluidlogged_api.mod.common.message.SyncFluidStatesMessage;
import git.jbredwards.fluidlogged_api.mod.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.block.model.ModelResourceLocation;
import net.minecraft.client.renderer.block.statemap.StateMapperBase;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.client.event.*;
import net.minecraftforge.client.model.ModelFluid;
import net.minecraftforge.client.model.ModelLoader;
import net.minecraftforge.common.model.TRSRTransformation;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.event.entity.player.FillBucketEvent;
import net.minecraftforge.event.world.ChunkWatchEvent;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.Event;
import net.minecraftforge.fml.common.eventhandler.EventPriority;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import net.minecraftforge.items.ItemHandlerHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@Mod.EventBusSubscriber(modid = Constants.MODID)
public final class EventHandler
{
    @SubscribeEvent(receiveCanceled = true)
    public static void attachFluidStates(@Nonnull AttachCapabilitiesEvent<Chunk> event) {
        event.addCapability(new ResourceLocation(Constants.MODID, "fluid_states"), new IFluidStateCapability.Provider());
    }

    @SuppressWarnings("deprecation")
    @SubscribeEvent
    public static void sendToPlayer(@Nonnull ChunkWatchEvent.Watch event) {
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(event.getChunkInstance());
        if(cap != null) Main.wrapper.sendTo(new SyncFluidStatesMessage(event.getChunk(), cap), event.getPlayer());
    }

    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOW)
    public static void removeBuiltInLiquidStateMappers(@Nullable TextureStitchEvent.Pre event) {
        Minecraft.getMinecraft().modelManager.getBlockModelShapes().getBlockStateMapper().setBuiltInBlocks.removeIf(b -> b instanceof BlockLiquid);
    }

    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOW)
    public static void registerLiquidStateMappers(@Nullable ModelRegistryEvent event) {
        for(Block block : ForgeRegistries.BLOCKS) {
            //allow vanilla fluid blocks to use the new fluid renderer
            if(block instanceof BlockLiquid) {
                ModelLoader.setCustomStateMapper(block, new StateMapperBase() {
                    @Nonnull
                    @Override
                    protected ModelResourceLocation getModelResourceLocation(@Nullable IBlockState state) {
                        return new ModelResourceLocation(Objects.requireNonNull(block.getRegistryName()), "fluid");
                    }
                });
            }
        }
    }

    @SuppressWarnings("ConstantConditions")
    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOW)
    public static void registerLiquidBakedModels(@Nonnull ModelBakeEvent event) {
        for(Block block : ForgeRegistries.BLOCKS) {
            if(block instanceof BlockLiquid) {
                IBakedModel model = new ModelFluid(getFluidFromBlock(block)).bake(TRSRTransformation.identity(), DefaultVertexFormats.ITEM, ModelLoader.defaultTextureGetter());
                event.getModelRegistry().putObject(new ModelResourceLocation(Objects.requireNonNull(block.getRegistryName()), "fluid"), model);
            }
        }
    }

    @SuppressWarnings("ConstantConditions")
    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.HIGH)
    public static void improveDebugScreen(@Nonnull RenderGameOverlayEvent.Text event) {
        final @Nullable RayTraceResult trace = Minecraft.getMinecraft().objectMouseOver;

        if(trace != null && trace.getBlockPos() != null && !event.getRight().isEmpty()) {
            final FluidState fluidState = FluidState.get(trace.getBlockPos());
            if(!fluidState.isEmpty()) {
                //separate the fluid info from the block info
                event.getRight().add("");
                //fluid block id
                event.getRight().add(String.valueOf(fluidState.getBlock() instanceof BlockLiquid
                        ? BlockLiquid.getStaticBlock(fluidState.getMaterial()).getRegistryName()
                        : fluidState.getBlock().getRegistryName()));
                //fluid id
                event.getRight().add("fluid: " + fluidState.getFluid().getName());
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGH)
    public static void bucketFluidlogging(@Nonnull FillBucketEvent event) {
        if(event.getResult() != Event.Result.DEFAULT) return;
        final EntityPlayer player = event.getEntityPlayer();

        //this does its own raytrace, because the one that comes with the event doesn't do what's need here
        final Vec3d playerVec = new Vec3d(player.posX, player.posY + player.getEyeHeight(), player.posZ);
        final @Nullable RayTraceResult trace = event.getWorld().rayTraceBlocks(playerVec, playerVec
                .add(player.getLookVec().scale(player.getEntityAttribute(EntityPlayer.REACH_DISTANCE).getAttributeValue())));

        if(trace == null) return;

        //don't try fluidlogging without a bucket
        final @Nullable IFluidHandlerItem handler = FluidUtil.getFluidHandler(ItemHandlerHelper.copyStackWithSize(event.getEmptyBucket(), 1));
        if(handler == null) return;

        final @Nullable FluidStack contained = handler.drain(Fluid.BUCKET_VOLUME, false);
        if(contained != null && !contained.getFluid().canBePlacedInWorld()) return;

        final BlockPos pos = trace.getBlockPos();
        final World world = event.getWorld();

        //if bucket is empty, try taking from the fluidlogged block
        if(contained == null) {
            if(tryBucketFill_Internal(world, pos, player, handler) || tryBucketFill_Internal(world, pos.offset(trace.sideHit), player, handler)) {
                event.setFilledBucket(handler.getContainer());
                event.setResult(Event.Result.ALLOW);
            }
        }

        //if bucket has a fluid, try fluidlogging it
        else if(tryBucketDrain_Internal(world, pos, player, handler, contained) || tryBucketDrain_Internal(world, pos.offset(trace.sideHit), player, handler, contained)) {
            event.setFilledBucket(handler.getContainer());
            event.setResult(Event.Result.ALLOW);
        }
    }

    private static boolean tryBucketFill_Internal(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EntityPlayer player, @Nonnull IFluidHandlerItem handler) {
        final FluidState fluidState = FluidState.get(world, pos);

        if(!fluidState.isEmpty() && handler.fill(fluidState.getBlock().drain(world, pos, true), true) == Fluid.BUCKET_VOLUME) {
            world.playSound(null, player.posX, player.posY + 0.5, player.posZ, fluidState.getFluid().getFillSound(), SoundCategory.BLOCKS, 1, 1);
            return true;
        }

        return false;
    }

    private static boolean tryBucketDrain_Internal(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EntityPlayer player, @Nonnull IFluidHandlerItem handler, @Nonnull FluidStack stack) {
        final IBlockState state = world.getBlockState(pos);
        final Fluid fluid = stack.getFluid();

        final boolean isFluidloggable = isFluidloggableFluid(fluid.getBlock()) && isStateFluidloggable(state, world, pos, fluid);
        if(isFluidloggable && handler.drain(new FluidStack(stack, FluidState.of(fluid).getBlock().place(world, pos, stack.copy(), true)), true) != null) {
            if(!world.provider.doesWaterVaporize() || !fluid.doesVaporize(stack))
                world.playSound(null, player.posX, player.posY + 0.5, player.posZ, fluid.getEmptySound(stack), SoundCategory.BLOCKS, 1, 1);

            return true;
        }

        return false;
    }
}
