/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.mod.client.optifine.OptifineCustomWaterColors;
import git.jbredwards.fluidlogged_api.mod.common.capability.util.FluidStateStorage;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandReloadConfig;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandSetFluidState;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import git.jbredwards.fluidlogged_api.mod.common.datafix.FluidloggedAPIFixableData;
import git.jbredwards.fluidlogged_api.mod.common.datafix.LegacyDataFixer;
import git.jbredwards.fluidlogged_api.mod.common.datafix.ToFluidloggedDataFixer;
import git.jbredwards.fluidlogged_api.mod.common.message.*;
import net.minecraft.block.BlockDispenser;
import net.minecraft.init.Items;
import net.minecraft.util.datafix.FixTypes;
import net.minecraftforge.common.ForgeModContainer;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.fluids.DispenseFluidContainer;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.common.*;
import net.minecraftforge.fml.common.event.*;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;

import java.io.IOException;
import java.util.OptionalInt;

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

    // Mod Compatibility
    public static final boolean
            isAquaAcrobatics = Loader.isModLoaded("aquaacrobatics"),
            isBetterFoliage = Loader.isModLoaded("betterfoliage"),
            isChiseledMe    = Loader.isModLoaded("chiseled_me"),
            isCubicChunks   = Loader.isModLoaded("cubicchunks"),
            isDynamicLights = Loader.isModLoaded("dynamiclights");

    @Mod.EventHandler
    static void preInit(@Nonnull final FMLPreInitializationEvent event) {
        // register capability
        CapabilityManager.INSTANCE.register(IFluidStateCapability.class, FluidStateStorage.INSTANCE, () -> null);
        // register packets
        WRAPPER.registerMessage(SMessageSyncFluidState.Handler.INSTANCE, SMessageSyncFluidState.class, 1, Side.CLIENT);
        WRAPPER.registerMessage(SMessageSyncFluidStates.Handler.INSTANCE, SMessageSyncFluidStates.class, 2, Side.CLIENT);
        WRAPPER.registerMessage(SMessageVaporizeEffects.Handler.INSTANCE, SMessageVaporizeEffects.class, 3, Side.CLIENT);
        WRAPPER.registerMessage(CMessageSyncGameRule.Handler.INSTANCE, CMessageSyncGameRule.class, 4, Side.SERVER);
        WRAPPER.registerMessage(SMessageSyncGameRule.Handler.INSTANCE, SMessageSyncGameRule.class, 5, Side.CLIENT);
    }

    @SideOnly(Side.CLIENT)
    @Mod.EventHandler
    static void preInitClient(@Nonnull final FMLPreInitializationEvent event) {
        // optifine custom water colors
        if(FMLClientHandler.instance().hasOptifine()) OptifineCustomWaterColors.setWaterColorHelper();
    }

    @Mod.EventHandler
    static void init(@Nonnull final FMLInitializationEvent event) throws IOException {
        // misc config settings
        FluidloggedAPIConfigs.initConfigs(false);
        ForgeModContainer.fixVanillaCascading = true;
        // fix legacy world data
        FMLCommonHandler.instance().getDataFixer().init(MODID, FluidloggedAPIFixableData.DATA_VERSION).registerFix(FixTypes.CHUNK, new FluidloggedAPIFixableData());
        if(Loader.isModLoaded("tropicraft")) ToFluidloggedDataFixer.STATE_MAPPERS.add((blockName, blockID, blockMetadata) -> { // fix old tropicraft "pseudo-fluidlogged" fences
            if(blockMetadata < 2 && blockName.getNamespace().equals("tropicraft") && blockName.getPath().endsWith("_fence")) return Pair.of(OptionalInt.of(0), "tropicraft:water");
            else return null;
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
        event.registerServerCommand(new CommandReloadConfig());
        event.registerServerCommand(new CommandReloadConfig.Trimmed());
        event.registerServerCommand(new CommandReloadConfig.TrimmedAPI());
        event.registerServerCommand(new CommandSetFluidState());
    }

    @Mod.EventHandler
    static void aboutToStart(@Nonnull final FMLServerAboutToStartEvent event) { LegacyDataFixer.init(); }

    @Mod.EventHandler
    static void stopped(@Nonnull final FMLServerStoppedEvent event) { LegacyDataFixer.reset(); }
}
