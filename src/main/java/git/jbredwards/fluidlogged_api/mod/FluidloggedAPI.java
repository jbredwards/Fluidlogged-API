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

package git.jbredwards.fluidlogged_api.mod;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.IFluidEventListener;
import git.jbredwards.fluidlogged_api.mod.client.optifine.OptifineHelper;
import git.jbredwards.fluidlogged_api.mod.common.capability.util.FluidStateStorage;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandFluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandReloadConfig;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandSetFluidState;
import git.jbredwards.fluidlogged_api.mod.common.datafix.ToFluidloggedDataFixer;
import git.jbredwards.fluidlogged_api.mod.common.datafix.modded.DynamicTreesDataFixer;
import git.jbredwards.fluidlogged_api.mod.common.datafix.modded.GalacticraftDataFixer;
import git.jbredwards.fluidlogged_api.mod.common.datafix.modded.TropicraftDataFixer;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import git.jbredwards.fluidlogged_api.mod.common.config.handler.LegacyConfigHandler;
import git.jbredwards.fluidlogged_api.mod.common.datafix.FluidloggedAPIFixableData;
import git.jbredwards.fluidlogged_api.mod.common.datafix.LegacyDataFixer;
import git.jbredwards.fluidlogged_api.mod.common.message.*;
import net.minecraft.block.BlockDispenser;
import net.minecraft.client.Minecraft;
import net.minecraft.client.resources.I18n;
import net.minecraft.client.resources.IReloadableResourceManager;
import net.minecraft.init.Items;
import net.minecraftforge.client.resource.ISelectiveResourceReloadListener;
import net.minecraftforge.client.resource.VanillaResourceType;
import net.minecraftforge.common.ForgeModContainer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.DispenseFluidContainer;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.common.*;
import net.minecraftforge.fml.common.event.*;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import net.minecraftforge.fml.relauncher.ReflectionHelper;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.orecruncher.dsurround.event.BlockUpdateEvent;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.io.IOException;
import java.util.Optional;

/**
 *
 * @author jbred
 *
 */
@Mod(modid = FluidloggedAPI.MODID, name = "Fluidlogged API", version = "3.2.0",
     updateJSON = "https://api.modrinth.com/updates/fluidlogged-api/forge_updates.json",
     guiFactory = "git.jbredwards.fluidlogged_api.mod.client.config.gui.FluidloggedAPIGuiFactory")
public final class FluidloggedAPI
{
    // Mod Constants
    @Nonnull public static final String MODID = "fluidlogged_api";
    @Nonnull public static final Logger LOGGER = LogManager.getFormatterLogger(MODID);
    @Nonnull public static final SimpleNetworkWrapper WRAPPER = NetworkRegistry.INSTANCE.newSimpleChannel(MODID);
    @Nullable private static String creditsKey, descKey;

    // Mod Compatibility
    public static final boolean
            isAquaAcrobatics       = Loader.isModLoaded("aquaacrobatics"),
            isChiseledMe           = Loader.isModLoaded("chiseled_me"),
            isCubicChunks          = Loader.isModLoaded("cubicchunks"),
            isDynamicLights        = Loader.isModLoaded("dynamiclights"),
            isGalacticraft         = Loader.isModLoaded("galacticraftcore"),
            isImmersiveEngineering = Loader.isModLoaded("immersiveengineering"),
            isSubaquatic           = Loader.isModLoaded("subaquatic");

    @Mod.EventHandler
    static void preInit(@Nonnull final FMLPreInitializationEvent event) {
        // register capability
        CapabilityManager.INSTANCE.register(IFluidStateCapability.class, FluidStateStorage.INSTANCE, () -> { throw new UnsupportedOperationException(); });
        // register packets
        WRAPPER.registerMessage(SMessageSyncFluidState.Handler.INSTANCE, SMessageSyncFluidState.class, 1, Side.CLIENT);
        WRAPPER.registerMessage(SMessageSyncFluidStates.Handler.INSTANCE, SMessageSyncFluidStates.class, 2, Side.CLIENT);
        WRAPPER.registerMessage(SMessageVaporizeEffects.Handler.INSTANCE, SMessageVaporizeEffects.class, 3, Side.CLIENT);
        WRAPPER.registerMessage(CMessageSyncGameRule.Handler.INSTANCE, CMessageSyncGameRule.class, 4, Side.SERVER);
        WRAPPER.registerMessage(SMessageSyncGameRule.Handler.INSTANCE, SMessageSyncGameRule.class, 5, Side.CLIENT);
        WRAPPER.registerMessage(SMessageCommandPrint.Handler.INSTANCE, SMessageCommandPrint.class, 6, Side.CLIENT);
        WRAPPER.registerMessage(SMessageSyncConfigs.Handler.INSTANCE, SMessageSyncConfigs.class, 7, Side.CLIENT);
    }

    @SideOnly(Side.CLIENT)
    @Mod.EventHandler
    static void preInitClient(@Nonnull final FMLPreInitializationEvent event) {
        // optifine custom water colors
        if(FMLClientHandler.instance().hasOptifine()) OptifineHelper.setWaterColorHelper();
    }

    @Mod.EventHandler
    static void init(@Nonnull final FMLInitializationEvent event) {
        // fix certain weird lighting issues with fluidlogged blocks
        ForgeRegistries.BLOCKS.getValuesCollection().stream().filter(FluidloggedUtils::isFluid).forEach(b -> b.useNeighborBrightness = true);
        // register FluidState listener for dynamic surroundings
        if(Loader.isModLoaded("dsurround")) IFluidEventListener.LISTENERS.add((chunk, pos, oldState, newState, flags) -> {
            if((flags & Constants.BlockFlags.SEND_TO_CLIENTS) != 0 && (!chunk.getWorld().isRemote || (flags &  Constants.BlockFlags.NO_RERENDER) == 0) && chunk.isPopulated())
                MinecraftForge.EVENT_BUS.post(new BlockUpdateEvent(chunk.getWorld(), pos, oldState.getState(), newState.getState(), flags));
        });
        // register legacy to-FluidState adapters
        if(Loader.isModLoaded("dynamictrees")) DynamicTreesDataFixer.register(); // fix old dynamictrees "pseudo-fluidlogged" roots
        if(Loader.isModLoaded("tropicraft")) TropicraftDataFixer.register(); // fix old tropicraft "pseudo-fluidlogged" fences
        if(isGalacticraft) GalacticraftDataFixer.register(); // fix old galacticraft "pseudo-fluidlogged" grating
    }

    @SideOnly(Side.CLIENT)
    @Mod.EventHandler
    static void initClient(@Nonnull final FMLInitializationEvent event) {
        Optional.ofNullable(Loader.instance().getIndexedModList().get(MODID)).ifPresent(mod -> {
            // remove "disable" button in mod gui
            ReflectionHelper.setPrivateValue(FMLModContainer.class, (FMLModContainer)mod, ModContainer.Disableable.NEVER, "disableability");
            // allow this mod's description and credits to be translated
            ((IReloadableResourceManager)Minecraft.getMinecraft().getResourceManager()).registerReloadListener((ISelectiveResourceReloadListener)(manager, condition) -> {
                if(condition.test(VanillaResourceType.LANGUAGES) && mod.getMetadata() != null) {
                    mod.getMetadata().credits = I18n.format(creditsKey == null ? creditsKey = mod.getMetadata().credits : creditsKey).replace("\\n", "\n");
                    mod.getMetadata().description = I18n.format(descKey == null ? descKey = mod.getMetadata().description : descKey);
                }
            });
        });
    }

    @Mod.EventHandler
    static void postInit(@Nonnull final FMLPostInitializationEvent event) {
        // fixes the vanilla bucket dispenser actions by replacing them with the forge one
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.LAVA_BUCKET, DispenseFluidContainer.getInstance());
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.WATER_BUCKET, DispenseFluidContainer.getInstance());
    }

    @Mod.EventHandler
    static void loadComplete(@Nonnull final FMLLoadCompleteEvent event) throws IOException {
        // fix legacy world data
        FluidloggedAPIFixableData.register("legacy_adapter", FluidloggedAPIFixableData.LEGACY_DATA_VERSION, LegacyDataFixer::fix);
        FluidloggedAPIFixableData.register("to_fluid_state", FluidloggedAPIFixableData.getToFluidStateDataVersion(), ToFluidloggedDataFixer::fix);
        // fix old config data if present
        LegacyConfigHandler.convertOldFile();
    }

    @SideOnly(Side.CLIENT)
    @Mod.EventHandler
    static void loadCompleteClient(@Nonnull final FMLLoadCompleteEvent event) {
        // load Optifine reflection classes
        if(FMLClientHandler.instance().hasOptifine()) OptifineHelper.onLoadComplete();
    }

    @Mod.EventHandler
    static void start(@Nonnull final FMLServerStartingEvent event) {
        // register commands
        event.registerServerCommand(new CommandSetFluidState(null));
        event.registerServerCommand(new CommandReloadConfig(null, "reloadFluidloggedAPI"));
        event.registerServerCommand(new CommandFluidloggedAPI("fluidlogged_api"));
        event.registerServerCommand(new CommandFluidloggedAPI("fluidlogged"));
    }

    @Mod.EventHandler
    static void aboutToStart(@Nonnull final FMLServerAboutToStartEvent event) throws IOException {
        if(isGalacticraft) GalacticraftDataFixer.init();
        LegacyDataFixer.init();
        // config settings
        ForgeModContainer.fixVanillaCascading = true;
        FluidloggedAPIConfigs.initConfigs(event.getServer(), false);
    }

    @Mod.EventHandler
    static void stopped(@Nonnull final FMLServerStoppedEvent event) {
        if(isGalacticraft) GalacticraftDataFixer.reset();
        LegacyDataFixer.reset();
    }
}
