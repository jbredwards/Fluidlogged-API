package git.jbredwards.fluidlogged_api.mod;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.network.message.MessageFluidState;
import git.jbredwards.fluidlogged_api.mod.client.optifine.OptifineCustomWaterColors;
import git.jbredwards.fluidlogged_api.mod.client.optifine.UnsupportedOptifineException;
import git.jbredwards.fluidlogged_api.mod.common.capability.FluidStateCapabilityVanilla;
import git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks.FluidStateCapabilityIColumn;
import git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks.FluidStateCapabilityICube;
import git.jbredwards.fluidlogged_api.mod.common.capability.util.FluidStateStorage;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandReloadConfig;
import git.jbredwards.fluidlogged_api.mod.common.command.CommandSetFluidState;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigHandler;
import git.jbredwards.fluidlogged_api.mod.common.legacy.LegacyDataFixer;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageReloadConfig;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageSyncFluidStates;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageVaporizeEffects;
import net.minecraft.block.BlockDispenser;
import net.minecraft.init.Items;
import net.minecraft.util.datafix.FixTypes;
import net.minecraftforge.common.ForgeModContainer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.fluids.DispenseFluidContainer;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.common.*;
import net.minecraftforge.fml.common.event.*;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;

import java.io.IOException;

import static git.jbredwards.fluidlogged_api.api.network.FluidloggedAPINetworkHandler.INSTANCE;

/**
 *
 * @author jbred
 *
 */
@Mod(modid = FluidloggedAPI.MODID, name = FluidloggedAPI.NAME, version = FluidloggedAPI.VERSION)
public final class FluidloggedAPI
{
    //mod id constants
    @Nonnull
    public static final String MODID = "fluidlogged_api", NAME  = "Fluidlogged API", VERSION = "2.2.5";
    public static final boolean //compat id constants
            isChiseledMe    = Loader.isModLoaded("chiseled_me"),
            isCubicChunks   = Loader.isModLoaded("cubicchunks"),
            isDynamicLights = Loader.isModLoaded("dynamiclights");

    @Mod.EventHandler
    static void preInit(@Nonnull FMLPreInitializationEvent event) {
        //register capability
        CapabilityManager.INSTANCE.register(IFluidStateCapability.class, FluidStateStorage.INSTANCE, () -> null);
        //register packets
        INSTANCE = NetworkRegistry.INSTANCE.newSimpleChannel(MODID);
        INSTANCE.registerMessage(MessageFluidState.Handler.INSTANCE, MessageFluidState.class, 1, Side.CLIENT);
        INSTANCE.registerMessage(MessageSyncFluidStates.Handler.INSTANCE, MessageSyncFluidStates.class, 2, Side.CLIENT);
        INSTANCE.registerMessage(MessageReloadConfig.Handler.INSTANCE, MessageReloadConfig.class, 3, Side.CLIENT);
        INSTANCE.registerMessage(MessageVaporizeEffects.Handler.INSTANCE, MessageVaporizeEffects.class, 4, Side.CLIENT);
        //no cubic chunks
        if(!isCubicChunks) MinecraftForge.EVENT_BUS.register(FluidStateCapabilityVanilla.class);
        //cubic chunks
        else {
            MinecraftForge.EVENT_BUS.register(FluidStateCapabilityIColumn.class);
            MinecraftForge.EVENT_BUS.register(FluidStateCapabilityICube.class);
        }
    }

    @SideOnly(Side.CLIENT)
    @Mod.EventHandler
    static void preInitClient(@Nonnull FMLPreInitializationEvent event) {
        UnsupportedOptifineException.checkOptifineVersion(); //make sure a supported version of optifine is installed
        if(FMLClientHandler.instance().hasOptifine()) OptifineCustomWaterColors.setWaterColorHelper(); //optifine custom water colors
    }

    @Mod.EventHandler
    static void init(@Nonnull FMLInitializationEvent event) {
        //fixes cascading world gen
        ForgeModContainer.fixVanillaCascading = true;
        //fixes the vanilla bucket dispenser actions by replacing them with the forge one
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.LAVA_BUCKET, DispenseFluidContainer.getInstance());
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.WATER_BUCKET, DispenseFluidContainer.getInstance());
        //fix legacy world data
        FMLCommonHandler.instance().getDataFixer()
                .init(MODID, LegacyDataFixer.DATA_VERSION)
                .registerFix(FixTypes.CHUNK, new LegacyDataFixer());
    }

    @Mod.EventHandler
    static void postInit(@Nonnull FMLPostInitializationEvent event) throws IOException {
        //finalize this mod's config
        FluidloggedAPIConfigHandler.complete();
    }

    @Mod.EventHandler
    static void start(@Nonnull FMLServerStartingEvent event) {
        event.registerServerCommand(new CommandSetFluidState());
        event.registerServerCommand(new CommandReloadConfig());
    }

    //initializes datafixer
    @Mod.EventHandler
    static void aboutToStart(@Nonnull FMLServerAboutToStartEvent event) { LegacyDataFixer.init(); }

    //resets datafixer
    @Mod.EventHandler
    static void stopped(@Nonnull FMLServerStoppedEvent event) { LegacyDataFixer.reset(); }
}
