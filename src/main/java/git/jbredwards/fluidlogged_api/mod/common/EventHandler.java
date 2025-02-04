/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common;

import git.jbredwards.fluidlogged_api.api.capability.CapabilityProvider;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.event.FluidloggableEvent;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigAccessor;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IHardcodedCapability;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.PluginChunk;
import git.jbredwards.fluidlogged_api.mod.common.capability.FluidStateCapabilityWrapped;
import git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks.FluidStateCapabilityICube;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import git.jbredwards.fluidlogged_api.mod.common.config.util.ConfigPredicate;
import git.jbredwards.fluidlogged_api.mod.common.message.CMessageSyncGameRule;
import git.jbredwards.fluidlogged_api.mod.common.message.SMessageSyncFluidStates;
import git.jbredwards.fluidlogged_api.mod.common.message.SMessageSyncGameRule;
import git.jbredwards.fluidlogged_api.mod.common.message.SMessageSyncRuntimeConfigs;
import io.github.opencubicchunks.cubicchunks.api.world.CubeWatchEvent;
import io.github.opencubicchunks.cubicchunks.api.world.ICube;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.network.NetHandlerPlayServer;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.ForgeModContainer;
import net.minecraftforge.common.config.Config;
import net.minecraftforge.common.config.ConfigManager;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.event.GameRuleChangeEvent;
import net.minecraftforge.event.world.ChunkWatchEvent;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.fml.client.event.ConfigChangedEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Optional;
import net.minecraftforge.fml.common.eventhandler.Event;
import net.minecraftforge.fml.common.eventhandler.EventPriority;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.network.FMLNetworkEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

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
    @SubscribeEvent
    static void configFileSync(@Nonnull final ConfigChangedEvent.OnConfigChangedEvent event) {
        if(FluidloggedAPI.MODID.equals(event.getModID())) ConfigManager.sync(FluidloggedAPI.MODID, Config.Type.INSTANCE);
    }

    @SubscribeEvent(priority = EventPriority.LOWEST)
    static void forceForgeCascadingFix(@Nonnull final ConfigChangedEvent.PostConfigChangedEvent event) {
        ForgeModContainer.fixVanillaCascading = true;
    }

    @SubscribeEvent(priority = EventPriority.HIGH)
    static void handleConfigOverrides(@Nonnull final FluidloggableEvent event) {
        // config settings for non-source fluidlogging if applicable
        if(!FluidloggedAPIConfig.nonSourceFluidlogging && event.fluid != null && !event.fluidState.isSource()) {
            event.setCanceled(true);
            event.setResult(Event.Result.DENY);
        }
        else {
            // config settings from actual state
            @Nonnull final IBlockState actualState = event.state.getActualState(event.world, event.pos);
            @Nullable final ConfigPredicate
                    blacklist = ((IConfigAccessor)actualState).getBlacklistPredicate(),
                    whitelist = ((IConfigAccessor)actualState).getWhitelistPredicate();

            // blacklist
            if(blacklist != null && blacklist.test(event.world, event.pos, actualState, event.fluidState)) {
                event.setCanceled(true);
                event.setResult(Event.Result.DENY);
            }

            // whitelist
            else if(whitelist != null && whitelist.test(event.world, event.pos, actualState, event.fluidState)) {
                event.setCanceled(true);
                event.setResult(Event.Result.ALLOW);
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGHEST)
    static void handleSyncConfigs(@Nonnull final FMLNetworkEvent.ServerConnectionFromClientEvent event) {
        if(event.getHandler() instanceof NetHandlerPlayServer) { // should always pass, but let's be safe
            @Nonnull final EntityPlayerMP player = ((NetHandlerPlayServer)event.getHandler()).player;
            if(!event.isLocal() || player.server.isDedicatedServer()) FluidloggedAPI.WRAPPER.sendTo(
                new SMessageSyncRuntimeConfigs(FluidloggedAPIConfigs.readConfigFiles(player.server)), player);
        }
    }

    @SubscribeEvent(priority = EventPriority.LOWEST)
    static void handleSyncDoFireTick(@Nonnull final GameRuleChangeEvent event) {
        if("doFireTick".equals(event.getRuleName())) FluidloggedAPI.WRAPPER.sendToAll(
            new SMessageSyncGameRule("doFireTick", event.getRules().getString("doFireTick"), true));
    }

    @SideOnly(Side.CLIENT)
    @SubscribeEvent(priority = EventPriority.LOWEST)
    static void handleSyncDoFireTick(@Nonnull final WorldEvent.Load event) {
        FluidloggedAPI.WRAPPER.sendToServer(new CMessageSyncGameRule("doFireTick", false));
    }

    // ============
    // DATA HANDLER
    // ============

    @SubscribeEvent(priority = EventPriority.LOW)
    static void attachToChunk(@Nonnull final AttachCapabilitiesEvent<Chunk> event) {
        if(!event.getCapabilities().containsKey(IFluidStateCapability.CAPABILITY_ID)) event.addCapability(IFluidStateCapability.CAPABILITY_ID,
            new CapabilityProvider<>(IFluidStateCapability.CAPABILITY, new FluidStateCapabilityWrapped((IHardcodedCapability)event.getObject()))
        );
    }

    @Optional.Method(modid = "cubicchunks")
    @SubscribeEvent(priority = EventPriority.LOW)
    static void attachToCube(@Nonnull final AttachCapabilitiesEvent<ICube> event) {
        if(!event.getCapabilities().containsKey(IFluidStateCapability.CAPABILITY_ID)) event.addCapability(IFluidStateCapability.CAPABILITY_ID,
            new CapabilityProvider<>(IFluidStateCapability.CAPABILITY, new FluidStateCapabilityICube(event.getObject()))
        );
    }

    @SubscribeEvent(priority = EventPriority.HIGHEST)
    static void syncChunk(@Nonnull final ChunkWatchEvent.Watch event) {
        @Nullable final Chunk chunk = event.getChunkInstance();
        if(chunk != null && !(FluidloggedAPI.isCubicChunks && PluginChunk.CCHooks.isCubicWorld(chunk.getWorld()))) {
            FluidloggedAPI.WRAPPER.sendTo(new SMessageSyncFluidStates(chunk, ((IHardcodedCapability)chunk).getFluidStateCapability()), event.getPlayer());
        }
    }

    @Optional.Method(modid = "cubicchunks")
    @SubscribeEvent(priority = EventPriority.HIGHEST)
    static void syncCube(@Nonnull final CubeWatchEvent event) {
        @Nullable final ICube cube = event.getCube();
        @Nullable final IFluidStateCapability cap = IFluidStateCapability.get(cube);
        if(cap != null) FluidloggedAPI.WRAPPER.sendTo(new SMessageSyncFluidStates(cube.getX(), cube.getY() << 4, cube.getZ(), cap), event.getPlayer());
    }
}
