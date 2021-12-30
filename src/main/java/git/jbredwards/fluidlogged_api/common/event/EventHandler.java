package git.jbredwards.fluidlogged_api.common.event;

import git.jbredwards.fluidlogged_api.Constants;
import git.jbredwards.fluidlogged_api.Main;
import git.jbredwards.fluidlogged_api.asm.replacements.BlockLiquidBase;
import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.network.SyncFluidStatesMessage;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
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
import net.minecraftforge.fluids.FluidRegistry;
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
import java.util.Optional;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mod.EventBusSubscriber(modid = Constants.MODID)
public final class EventHandler
{
    @SubscribeEvent
    public static void attachCapability(@Nonnull AttachCapabilitiesEvent<Chunk> event) {
        event.addCapability(new ResourceLocation(Constants.MODID, "fluid_states"), new IFluidStateCapability.Provider());
    }

    @SuppressWarnings("deprecation")
    @SubscribeEvent
    public static void sendToPlayer(@Nonnull ChunkWatchEvent.Watch event) {
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(event.getChunkInstance());
        if(cap != null) Main.wrapper.sendTo(new SyncFluidStatesMessage(event.getChunk(), cap.getFluidStates()), event.getPlayer());
    }

    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOWEST, receiveCanceled = true)
    public static void removeBuiltInLiquidStateMappers(@Nullable TextureStitchEvent.Pre event) {
        Minecraft.getMinecraft().modelManager.getBlockModelShapes().getBlockStateMapper().setBuiltInBlocks.removeIf(b -> b instanceof BlockLiquidBase);
    }

    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOWEST, receiveCanceled = true)
    public static void registerLiquidStateMappers(@Nullable ModelRegistryEvent event) {
        for(Block block : ForgeRegistries.BLOCKS) {
            if(block instanceof BlockLiquidBase) {
                ModelLoader.setCustomStateMapper(block, new StateMapperBase() {
                    @Nonnull
                    @Override
                    protected ModelResourceLocation getModelResourceLocation(@Nonnull IBlockState state) {
                        return new ModelResourceLocation(Objects.requireNonNull(state.getBlock().getRegistryName()), "fluid");
                    }
                });
            }
        }
    }

    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOWEST, receiveCanceled = true)
    public static void registerLiquidBakedModels(@Nonnull ModelBakeEvent event) {
        for(Block block : ForgeRegistries.BLOCKS) {
            if(block instanceof BlockLiquidBase) {
                IBakedModel model = new ModelFluid(Optional.ofNullable(FluidloggedUtils.getFluidFromBlock(block)).orElse(FluidRegistry.WATER)).bake(TRSRTransformation.identity(), DefaultVertexFormats.ITEM, ModelLoader.defaultTextureGetter());
                event.getModelRegistry().putObject(new ModelResourceLocation(Objects.requireNonNull(block.getRegistryName()), "fluid"), model);
            }
        }
    }

    @SuppressWarnings("ConstantConditions")
    @SideOnly(Side.CLIENT)
    @SubscribeEvent
    public static void improveDebugScreen(@Nonnull RenderGameOverlayEvent.Text event) {
        final @Nullable RayTraceResult trace = Minecraft.getMinecraft().objectMouseOver;

        if(trace != null && trace.getBlockPos() != null && !event.getRight().isEmpty()) {
            final FluidState fluidState = FluidState.get(trace.getBlockPos());
            if(!fluidState.isEmpty()) {
                event.getRight().add("");
                event.getRight().add("fluid:" + fluidState.getFluid().getName());
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGH)
    public static void bucketFluidlogging(@Nonnull FillBucketEvent event) {
        if(event.getResult() != Event.Result.DEFAULT || event.getTarget() == null) return;

        final @Nullable IFluidHandlerItem handler = FluidUtil.getFluidHandler(ItemHandlerHelper.copyStackWithSize(event.getEmptyBucket(), 1));
        if(handler == null) return;

        final @Nullable FluidStack contained = handler.drain(Fluid.BUCKET_VOLUME, false);
        final BlockPos pos = event.getTarget().getBlockPos();
        final World world  = event.getWorld();

        final FluidState fluidState = FluidState.get(world, pos);
        final IBlockState state = world.getBlockState(pos);
        final EntityPlayer player = event.getEntityPlayer();

        //if bucket is empty, try taking from the fluidlogged block
        if(contained == null) {
            if(!fluidState.isEmpty() && FluidloggedUtils.setFluidState(world, pos, state, FluidState.EMPTY, false, 3)) {
                world.playSound(null, player.posX, player.posY + 0.5, player.posZ, fluidState.getFluid().getFillSound(), SoundCategory.BLOCKS, 1, 1);
                handler.fill(new FluidStack(fluidState.getFluid(), Fluid.BUCKET_VOLUME), true);

                //event bucket
                event.setFilledBucket(handler.getContainer());
                event.setResult(Event.Result.ALLOW);
            }
        }

        //if bucket has a fluid, try fluidlogging it
        else if(FluidloggedUtils.isFluidFluidloggable(contained.getFluid().getBlock())
                && FluidloggedUtils.isStateFluidloggable(state, contained.getFluid())
                && FluidloggedUtils.setFluidState(world, pos, state, FluidState.of(contained.getFluid()), true, 3)) {

            world.playSound(null, player.posX, player.posY + 0.5, player.posZ, contained.getFluid().getEmptySound(), SoundCategory.BLOCKS, 1, 1);
            handler.drain(contained, true);

            //event bucket
            event.setFilledBucket(handler.getContainer());
            event.setResult(Event.Result.ALLOW);
        }
    }

    //useful while debugging
    /*@SubscribeEvent
    public static void debugStick(@Nonnull PlayerInteractEvent.RightClickBlock event) {
        if(event.getEntityPlayer().getHeldItemMainhand().getItem() == Items.STICK)
            FluidloggedUtils.setFluidState(event.getWorld(), event.getPos(), null, FluidState.of(FluidRegistry.WATER), true, 3);

        else if(event.getEntityPlayer().getHeldItemMainhand().getItem() == Items.BLAZE_ROD)
            System.out.println(FluidState.get(event.getWorld(), event.getPos()));
    }*/
}
