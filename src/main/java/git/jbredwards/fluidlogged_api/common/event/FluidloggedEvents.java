package git.jbredwards.fluidlogged_api.common.event;

import git.jbredwards.fluidlogged_api.Fluidlogged;
import git.jbredwards.fluidlogged_api.common.block.AbstractFluidloggedBlock;
import git.jbredwards.fluidlogged_api.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged_api.util.FluidloggedConstants;
import git.jbredwards.fluidlogged_api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.common.block.TileEntityFluidlogged;
import net.minecraft.advancements.CriteriaTriggers;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.SoundType;
import net.minecraft.block.material.Material;
import net.minecraft.block.properties.IProperty;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.block.model.ModelResourceLocation;
import net.minecraft.client.renderer.block.statemap.StateMapperBase;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.init.Blocks;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.*;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.World;
import net.minecraftforge.client.event.ModelBakeEvent;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.event.RenderGameOverlayEvent;
import net.minecraftforge.client.event.TextureStitchEvent;
import net.minecraftforge.client.model.ModelFluid;
import net.minecraftforge.client.model.ModelLoader;
import net.minecraftforge.common.model.TRSRTransformation;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.event.entity.player.FillBucketEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.Event;
import net.minecraftforge.fml.common.eventhandler.EventPriority;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.TickEvent;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import net.minecraftforge.fml.common.registry.GameRegistry;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.*;

import static net.minecraftforge.fluids.BlockFluidBase.LEVEL;

/**
 *
 * @author jbred
 *
 */
@Mod.EventBusSubscriber(modid = FluidloggedConstants.MODID)
public class FluidloggedEvents
{
    //registers the water & lava fluidlogged te's
    @SuppressWarnings("unused")
    @SubscribeEvent
    public static void registerBlocks(RegistryEvent.Register<Block> event) {
        //sets up the registry names
        Fluidlogged.WATERLOGGED_TE.setRegistryName("waterlogged_te").setUnlocalizedName("water");
        Fluidlogged.LAVALOGGED_TE.setRegistryName("lavalogged_te").setUnlocalizedName("lava");
        //registers the blocks
        event.getRegistry().registerAll(Fluidlogged.WATERLOGGED_TE, Fluidlogged.LAVALOGGED_TE);
        //registers the te
        GameRegistry.registerTileEntity(TileEntityFluidlogged.class, new ResourceLocation(FluidloggedConstants.MODID, "te"));
    }

    //smoothwater mod integration
    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOWEST)
    public static void onModelRegistry(ModelRegistryEvent event) {
        //remove fluidlogged fluid block models
        FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.values().forEach(block -> ModelLoader.setCustomStateMapper(block, b -> new HashMap<>()));
        //register vanilla fluid block models
        for(Block block : ForgeRegistries.BLOCKS) {
            if(block instanceof BlockLiquid) {
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

    //smoothwater mod integration
    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOWEST)
    public static void onTextureStitch(TextureStitchEvent.Pre event) {
        Minecraft.getMinecraft().modelManager.getBlockModelShapes().getBlockStateMapper().setBuiltInBlocks.removeIf(b -> b instanceof BlockLiquid);
    }

    //smoothwater mod integration
    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOWEST)
    public static void onModelBake(ModelBakeEvent event) {
        for(Block block : ForgeRegistries.BLOCKS) {
            if(block instanceof BlockLiquid) {
                IBakedModel model = new ModelFluid(Optional.ofNullable(FluidloggedUtils.getFluidFromBlock(block)).orElse(FluidRegistry.WATER)).bake(TRSRTransformation.identity(), DefaultVertexFormats.ITEM, ModelLoader.defaultTextureGetter());
                event.getModelRegistry().putObject(new ModelResourceLocation(Objects.requireNonNull(block.getRegistryName()), "fluid"), model);
            }
        }
    }

    //shows fluidlogged barrier particles
    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    @SubscribeEvent
    public static void showFluidloggedBarrier(TickEvent.ClientTickEvent event) {
        final Minecraft mc = Minecraft.getMinecraft();
        final @Nullable EntityPlayerSP player = mc.player;
        final @Nullable WorldClient world = mc.world;

        if(player != null && world != null) {
            final BlockPos origin = new BlockPos(player);

            if(player.isCreative() && world.getTotalWorldTime() % 80 == 0 && player.getHeldItemMainhand().getItem() == Item.getItemFromBlock(Blocks.BARRIER)) {
                for(int x = -32; x < 32; x++) {
                    for(int y = -32; y < 32; y++) {
                        for(int z = -32; z < 32; z++) {
                            BlockPos pos = origin.add(x, y, z);
                            @Nullable TileEntity te = world.getTileEntity(pos);

                            boolean flag = (te instanceof TileEntityFluidlogged && ((TileEntityFluidlogged)te).stored.getBlock() == Blocks.BARRIER);
                            if(flag) world.spawnParticle(EnumParticleTypes.BARRIER, pos.getX() + 0.5, pos.getY() + 0.5, pos.getZ() + 0.5, 0, 0, 0);
                        }
                    }
                }
            }
        }
    }

    //shows more fluidlogged block data in the F3 screen
    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    @SubscribeEvent
    public static void showAdditionalDebugData(RenderGameOverlayEvent.Text event) {
        final Minecraft mc = Minecraft.getMinecraft();
        final WorldClient world = mc.world;
        final RayTraceResult trace = mc.objectMouseOver;

        if(world != null && trace != null && trace.getBlockPos() != null && !event.getLeft().isEmpty() && !event.getRight().isEmpty()) {
            @Nullable IBlockState stored = FluidloggedUtils.getStored(world, trace.getBlockPos());
            if(stored != null) {
                //some blocks like to hide additional data in their actual state
                stored = stored.getActualState(world, trace.getBlockPos());
                //initial text
                event.getRight().add("");
                event.getRight().add(Optional.ofNullable(stored.getBlock().getRegistryName()).orElse(new ResourceLocation("barrier")).toString());
                //block state data
                for(Map.Entry<IProperty<?>, Comparable<?>> entry : stored.getProperties().entrySet()) {
                    String value = entry.getValue().toString();
                    //consistent coloring with vanilla
                    if(value.equals("true")) value = TextFormatting.GREEN + value;
                    else if(value.equals("false")) value = TextFormatting.RED + value;
                    //adds the text
                    event.getRight().add(entry.getKey().getName() + ": " + value);
                }
            }
        }
    }

    //===================
    //FLUIDLOGGING EVENTS
    //===================

    //fired when a player tried to take from a fluidlogged te, or when they try to fluidlog a block
    @SuppressWarnings("unused")
    @SubscribeEvent(priority = EventPriority.HIGH)
    public static void fluidPlaceOrTake(FillBucketEvent event) {
        final @Nullable IFluidHandlerItem handler = FluidUtil.getFluidHandler(event.getEmptyBucket());

        //this shouldn't be null, but just in case
        if(handler != null && event.getTarget() != null) {
            //null if the bucket is empty
            final @Nullable FluidStack fluidStack = handler.drain(Fluid.BUCKET_VOLUME, false);
            final BlockPos pos = event.getTarget().getBlockPos();
            final World world = event.getWorld();
            final EnumFacing sideHit = event.getTarget().sideHit;

            //if the fluidStack is empty, the bucket is empty
            if(fluidStack == null) {
                //directly interacts with the block
                if(runBucketEmpty(event, world, world.getBlockState(pos), pos, event.getEntityPlayer())) return;
                //indirectly interacts with the block
                runBucketEmpty(event, world, world.getBlockState(pos.offset(sideHit)), pos.offset(sideHit), event.getEntityPlayer());
            }
            //since the bucket if full, the block will get fluidlogged
            else {
                //directly interacts with the block
                if(runBucketFull(event, world, world.getBlockState(pos), pos, event.getEntityPlayer(), fluidStack)) return;
                //indirectly interacts with the block
                runBucketFull(event, world, world.getBlockState(pos.offset(sideHit)), pos.offset(sideHit), event.getEntityPlayer(), fluidStack);
            }
        }
    }

    public static boolean runBucketEmpty(FillBucketEvent event, World world, IBlockState here, BlockPos pos, EntityPlayer player) {
        //if the state is fluidlogged
        if(here.getBlock() instanceof BlockFluidloggedTE && FluidloggedUtils.tryUnfluidlogBlock(world, pos, here, FluidloggedUtils.getStored(world, pos))) {
            //fill sound
            final Fluid fluid = ((BlockFluidloggedTE)here.getBlock()).fluid;
            final SoundEvent sound = fluid.getFillSound(new FluidStack(fluid, Fluid.BUCKET_VOLUME));
            world.playSound(null, player.posX, player.posY + 0.5, player.posZ, sound, SoundCategory.BLOCKS, 1, 1);

            //event bucket
            event.setFilledBucket(FluidloggedUtils.getFilledBucket(event.getEmptyBucket(), fluid));
            event.setResult(Event.Result.ALLOW);

            return true;
        }
        //if the block here is a normal fluidlogged block, the block should not be drained
        else if(here.getBlock() instanceof AbstractFluidloggedBlock && !here.getBlock().isReplaceable(world, pos)) {
            event.setResult(Event.Result.DENY);
            event.setCanceled(true);

            return true;
        }

        //nothing happened
        return false;
    }

    public static boolean runBucketFull(FillBucketEvent event, World world, IBlockState here, BlockPos pos, EntityPlayer player, FluidStack fluidStack) {
        //if the state can be fluidlogged with the given fluid
        if(FluidloggedUtils.tryFluidlogBlock(world, pos, here, fluidStack.getFluid(), false)) {
            //empty sound
            final SoundEvent sound = fluidStack.getFluid().getEmptySound(fluidStack);
            world.playSound(null, player.posX, player.posY + 0.5, player.posZ, sound, SoundCategory.BLOCKS, 1, 1);

            //event bucket
            event.setFilledBucket(FluidloggedUtils.getEmptyBucket(event.getEmptyBucket()));
            event.setResult(Event.Result.ALLOW);

            return true;
        }
        //if the block here should not get replaced with fluid
        else if(here.getBlock() instanceof AbstractFluidloggedBlock && !here.getBlock().isReplaceable(world, pos)) {
            event.setResult(Event.Result.DENY);
            event.setCanceled(true);

            return true;
        }

        //nothing happened
        return false;
    }

    //allows the player to place fluidloggable blocks into fluid and have them become fluidlogged
    @SuppressWarnings("unused")
    @SubscribeEvent
    public static void placeBlockInFluid(PlayerInteractEvent.RightClickBlock event) {
        final @Nullable EnumFacing facing = event.getFace();
        final ItemStack held = event.getItemStack();

        //if the held item is a block
        if(facing != null) {
            final World world = event.getWorld();
            final BlockPos pos = event.getPos().offset(facing);
            final IBlockState here = world.getBlockState(pos);
            final @Nullable Fluid fluid = getFluid(here);

            //if the block is a fluid
            if(fluid != null && !(here.getBlock() instanceof AbstractFluidloggedBlock)) {
                final int meta = held.getItem().getMetadata(held.getMetadata());
                final Vec3d hit = Optional.ofNullable(event.getHitVec()).orElse(new Vec3d(pos));
                final EntityPlayer player = event.getEntityPlayer();
                final IBlockState stored = Block.getBlockFromItem(held.getItem()).getStateForPlacement(world, pos, facing, (float)hit.x - pos.getX(), (float)hit.y - pos.getY(), (float)hit.z - pos.getZ(), meta, player, event.getHand());

                if(FluidloggedUtils.tryFluidlogBlock(world, pos, stored, fluid, true)) {
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

    //gets the fluid source here, null if none
    @Nullable
    public static Fluid getFluid(IBlockState state) {
        if((state.getBlock() instanceof BlockFluidClassic || state.getBlock() instanceof BlockLiquid) && state.getValue(LEVEL) == 0) {
            if(state.getBlock() instanceof BlockFluidClassic) return ((BlockFluidClassic)state.getBlock()).getFluid();
            else {
                if(state.getMaterial() == Material.WATER) return FluidRegistry.WATER;
                else return FluidRegistry.LAVA;
            }
        }

        return null;
    }
}
