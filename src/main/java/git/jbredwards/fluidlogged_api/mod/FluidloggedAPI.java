/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.client.optifine.OptifineCustomWaterColors;
import git.jbredwards.fluidlogged_api.mod.common.capability.util.FluidStateStorage;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandFluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandReloadConfig;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandSetFluidState;
import git.jbredwards.fluidlogged_api.mod.common.datafix.tropicraft.TropicraftDataFixer;
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
import net.minecraft.util.datafix.FixTypes;
import net.minecraftforge.client.resource.ISelectiveResourceReloadListener;
import net.minecraftforge.client.resource.VanillaResourceType;
import net.minecraftforge.common.ForgeModContainer;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.fluids.DispenseFluidContainer;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.common.*;
import net.minecraftforge.fml.common.event.*;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.io.IOException;
import java.util.Optional;

/**
 *
 * @author jbred
 *
 */
@Mod(modid = FluidloggedAPI.MODID, useMetadata = true,
     guiFactory = "git.jbredwards.fluidlogged_api.mod.client.gui.FluidloggedAPIGuiFactory")
public final class FluidloggedAPI
{
    // Mod Constants
    @Nonnull public static final String MODID = "fluidlogged_api";
    @Nonnull public static final SimpleNetworkWrapper WRAPPER = NetworkRegistry.INSTANCE.newSimpleChannel(MODID);
    @Nullable private static String creditsKey, descKey;

    // Mod Compatibility
    public static final boolean
            isAquaAcrobatics = Loader.isModLoaded("aquaacrobatics"),
            isBetterFoliage = Loader.isModLoaded("betterfoliage"),
            isChiseledMe    = Loader.isModLoaded("chiseled_me"),
            isCubicChunks   = Loader.isModLoaded("cubicchunks"),
            isDynamicLights = Loader.isModLoaded("dynamiclights"),
            isSubaquatic    = Loader.isModLoaded("subaquatic");

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
        WRAPPER.registerMessage(SMessageSyncRuntimeConfigs.Handler.INSTANCE, SMessageSyncRuntimeConfigs.class, 7, Side.CLIENT);
    }

    @SideOnly(Side.CLIENT)
    @Mod.EventHandler
    static void preInitClient(@Nonnull final FMLPreInitializationEvent event) {
        // optifine custom water colors
        if(FMLClientHandler.instance().hasOptifine()) OptifineCustomWaterColors.setWaterColorHelper();
    }

    @Mod.EventHandler
    static void init(@Nonnull final FMLInitializationEvent event) throws IOException {
        // fix old config data if present
        LegacyConfigHandler.convertOldFile();
        // fix certain weird lighting issues with fluidlogged blocks
        ForgeRegistries.BLOCKS.getValuesCollection().stream().filter(FluidloggedUtils::isFluid).forEach(b -> b.useNeighborBrightness = true);
        // fix legacy world data
        FMLCommonHandler.instance().getDataFixer().init(MODID, FluidloggedAPIFixableData.DATA_VERSION).registerFix(FixTypes.CHUNK, new FluidloggedAPIFixableData());
        if(Loader.isModLoaded("tropicraft")) TropicraftDataFixer.register(); // fix old tropicraft "pseudo-fluidlogged" fences
    }

    @SideOnly(Side.CLIENT)
    @Mod.EventHandler
    static void initClient(@Nonnull final FMLInitializationEvent event) {
        // allow this mod's description and credits to be translated
        ((IReloadableResourceManager)Minecraft.getMinecraft().getResourceManager()).registerReloadListener((ISelectiveResourceReloadListener)(manager, condition) -> {
            if(condition.test(VanillaResourceType.LANGUAGES)) Optional.ofNullable(Loader.instance().getIndexedModList().get(MODID)).map(ModContainer::getMetadata).ifPresent(metadata -> {
                metadata.credits = I18n.format(creditsKey == null ? creditsKey = metadata.credits : creditsKey).replace("\\n", "\n");
                metadata.description = I18n.format(descKey == null ? descKey = metadata.description : descKey);
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
    static void start(@Nonnull final FMLServerStartingEvent event) {
        // register commands
        event.registerServerCommand(new CommandSetFluidState(null));
        event.registerServerCommand(new CommandReloadConfig(null, "reloadFluidloggedAPI"));
        event.registerServerCommand(new CommandFluidloggedAPI("fluidloggedAPI"));
        event.registerServerCommand(new CommandFluidloggedAPI("fluidlogged"));
    }

    @Mod.EventHandler
    static void aboutToStart(@Nonnull final FMLServerAboutToStartEvent event) throws IOException {
        LegacyDataFixer.init();
        // config settings
        ForgeModContainer.fixVanillaCascading = true;
        FluidloggedAPIConfigs.initConfigs(event.getServer(), false);
    }

    @Mod.EventHandler
    static void stopped(@Nonnull final FMLServerStoppedEvent event) {
        LegacyDataFixer.reset();
    }
}
