package git.jbredwards.fluidlogged_api.mod.client;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.block.model.ModelResourceLocation;
import net.minecraft.client.renderer.block.statemap.StateMapperBase;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.client.resources.I18n;
import net.minecraft.util.math.RayTraceResult;
import net.minecraftforge.client.event.ModelBakeEvent;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.event.RenderGameOverlayEvent;
import net.minecraftforge.client.event.TextureStitchEvent;
import net.minecraftforge.client.model.ModelFluid;
import net.minecraftforge.client.model.ModelLoader;
import net.minecraftforge.common.model.TRSRTransformation;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.EventPriority;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import net.minecraftforge.fml.relauncher.Side;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 *
 * @author jbred
 *
 */
@Mod.EventBusSubscriber(modid = FluidloggedAPI.MODID, value = Side.CLIENT)
public final class ClientEventHandler
{
    @SubscribeEvent(priority = EventPriority.LOW)
    static void removeBuiltInLiquidStateMappers(@Nonnull TextureStitchEvent.Pre event) {
        Minecraft.getMinecraft().modelManager.getBlockModelShapes().getBlockStateMapper()
                .setBuiltInBlocks.removeIf(b -> b instanceof BlockLiquid);
    }

    //allow vanilla liquid blocks to use the new fluid renderer
    @SubscribeEvent(priority = EventPriority.LOW)
    static void registerLiquidStateMappers(@Nonnull ModelRegistryEvent event) {
        for(Block block : ForgeRegistries.BLOCKS) {
            if(block instanceof BlockLiquid && FluidloggedUtils.isFluid(block)) {
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
    @SubscribeEvent(priority = EventPriority.LOW)
    static void registerLiquidBakedModels(@Nonnull ModelBakeEvent event) {
        for(Block block : ForgeRegistries.BLOCKS) {
            if(block instanceof BlockLiquid && FluidloggedUtils.isFluid(block)) {
                final ModelResourceLocation location = new ModelResourceLocation(block.getRegistryName(), "fluid");
                final IBakedModel model = new ModelFluid(FluidloggedUtils.getFluidFromBlock(block)).bake(
                    TRSRTransformation.identity(), DefaultVertexFormats.BLOCK, ModelLoader.defaultTextureGetter()
                );

                event.getModelRegistry().putObject(location, model);
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGH)
    static void improveDebugScreen(@Nonnull RenderGameOverlayEvent.Text event) {
        final @Nullable RayTraceResult trace = Minecraft.getMinecraft().objectMouseOver;
        if(trace != null && trace.typeOfHit == RayTraceResult.Type.BLOCK && !event.getRight().isEmpty()) {
            final FluidState fluidState = FluidState.get(trace.getBlockPos());
            if(!fluidState.isEmpty()) {
                //separate the fluid info from the block info
                event.getRight().add("");
                //fluid block id
                event.getRight().add(String.valueOf(fluidState.getBlock() instanceof BlockLiquid
                        ? BlockLiquid.getStaticBlock(fluidState.getMaterial()).getRegistryName()
                        : fluidState.getBlock().getRegistryName()));

                //fluid id
                event.getRight().add(I18n.format("debugOverlay.fluidloggedAPI", fluidState.getFluid().getName()));
            }
        }
    }
}
