/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.client;

import com.google.common.base.Functions;
import com.google.common.collect.Maps;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import net.minecraft.block.BlockLiquid;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.block.model.ModelResourceLocation;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.client.resources.I18n;
import net.minecraft.util.math.RayTraceResult;
import net.minecraftforge.client.event.ModelBakeEvent;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.event.RenderGameOverlayEvent;
import net.minecraftforge.client.model.ModelFluid;
import net.minecraftforge.client.model.ModelLoader;
import net.minecraftforge.common.model.TRSRTransformation;
import net.minecraftforge.fml.client.event.ConfigChangedEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.EventPriority;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import net.minecraftforge.fml.relauncher.Side;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
@Mod.EventBusSubscriber(modid = FluidloggedAPI.MODID, value = Side.CLIENT)
public final class ClientEventHandler
{
    @SubscribeEvent(priority = EventPriority.LOW)
    static void updateRenderOnConfigChange(@Nonnull final ConfigChangedEvent.PostConfigChangedEvent event) {
        if(event.isWorldRunning() && FluidloggedAPI.MODID.equals(event.getModID())) Minecraft.getMinecraft().renderGlobal.loadRenderers();
    }

    @SubscribeEvent(priority = EventPriority.HIGH)
    static void registerLiquidStateMappers(@Nonnull final ModelRegistryEvent event) {
        ForgeRegistries.BLOCKS.getEntries().forEach(entry -> {
            if(entry.getValue() instanceof BlockLiquid && FluidloggedUtils.isFluid(entry.getValue())) {
                ModelLoader.setCustomStateMapper(entry.getValue(), block ->
                        Maps.toMap(block.getBlockState().getValidStates(),
                        Functions.constant(new ModelResourceLocation(entry.getKey(), "fluid"))));
            }
        });
    }

    @SuppressWarnings("ConstantConditions")
    @SubscribeEvent(priority = EventPriority.LOW)
    static void registerLiquidBakedModels(@Nonnull final ModelBakeEvent event) {
        ForgeRegistries.BLOCKS.getEntries().forEach(entry -> {
            if(entry.getValue() instanceof BlockLiquid && FluidloggedUtils.isFluid(entry.getValue())) {
                @Nonnull final ModelResourceLocation location = new ModelResourceLocation(entry.getKey(), "fluid");
                @Nonnull final IBakedModel model = new ModelFluid(FluidloggedUtils.getFluidFromBlock(entry.getValue()))
                        .bake(TRSRTransformation.identity(), DefaultVertexFormats.BLOCK, ModelLoader.defaultTextureGetter());

                event.getModelRegistry().putObject(location, model);
                event.getModelManager().getBlockModelShapes().getBlockStateMapper().setBuiltInBlocks.remove(entry.getValue());
            }
        });
    }

    @SubscribeEvent(priority = EventPriority.HIGH)
    static void improveDebugScreen(@Nonnull final RenderGameOverlayEvent.Text event) {
        @Nullable final RayTraceResult trace = Minecraft.getMinecraft().objectMouseOver;
        if(trace != null && trace.typeOfHit == RayTraceResult.Type.BLOCK && !event.getRight().isEmpty()) {
            @Nonnull final FluidState fluidState = FluidState.get(trace.getBlockPos());
            if(fluidState != FluidState.EMPTY) {
                // separate the fluid info from the block info
                event.getRight().add("");
                // display FluidState block & fluid
                event.getRight().add(String.valueOf(fluidState.getBlock().getRegistryName()));
                event.getRight().add(I18n.format("debug.fluidlogged_api.fluid", fluidState.getFluid().getName()));
                // display FluidState level
                if(!fluidState.isEmpty()) event.getRight().add(I18n.format("debug.fluidlogged_api.level", fluidState.getLevel()));
            }
        }
    }
}
