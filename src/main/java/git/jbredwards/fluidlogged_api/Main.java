package git.jbredwards.fluidlogged_api;

import git.jbredwards.fluidlogged_api.common.config.ConfigHandler;
import git.jbredwards.fluidlogged_api.common.storage.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.network.FluidStateMessage;
import git.jbredwards.fluidlogged_api.common.network.SyncFluidStatesMessage;
import git.jbredwards.fluidlogged_api.common.storage.IChunkProvider;
import net.minecraft.block.BlockDispenser;
import net.minecraft.client.Minecraft;
import net.minecraft.init.Items;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.fluids.DispenseFluidContainer;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.SidedProxy;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;
import net.minecraftforge.fml.relauncher.Side;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.io.IOException;

import static git.jbredwards.fluidlogged_api.Constants.*;

/**
 *
 * @author jbred
 *
 */
@Mod(modid = MODID, name = NAME, version = VERSION)
public final class Main
{
    @SuppressWarnings("NotNullFieldNotInitialized")
    @SidedProxy(clientSide = "git.jbredwards.fluidlogged_api.Main$ClientProxy", serverSide = "git.jbredwards.fluidlogged_api.Main$CommonProxy")
    @Nonnull public static CommonProxy proxy;

    @SuppressWarnings("NotNullFieldNotInitialized")
    @Nonnull public static SimpleNetworkWrapper wrapper;

    @SuppressWarnings("unused")
    @Mod.EventHandler
    public static void preInit(@Nullable FMLPreInitializationEvent event) {
        //register capability
        CapabilityManager.INSTANCE.register(IFluidStateCapability.class, IFluidStateCapability.Storage.INSTANCE, IFluidStateCapability.Impl::new);
        //register packets
        wrapper = NetworkRegistry.INSTANCE.newSimpleChannel(MODID);
        wrapper.registerMessage(FluidStateMessage.Handler.INSTANCE, FluidStateMessage.class, 1, Side.CLIENT);
        wrapper.registerMessage(SyncFluidStatesMessage.Handler.INSTANCE, SyncFluidStatesMessage.class, 2, Side.CLIENT);
    }


    @SuppressWarnings("unused")
    @Mod.EventHandler
    public static void init(@Nullable FMLInitializationEvent event) throws IOException {
        //initialize this mod's config
        ConfigHandler.initialize();
        //fixes the vanilla bucket dispenser actions by replacing them with the forge one
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.WATER_BUCKET, DispenseFluidContainer.getInstance());
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.LAVA_BUCKET,  DispenseFluidContainer.getInstance());
    }

    //handles server-side code
    public static class CommonProxy
    {
        //tries to get the chunk stored in the IBlockAccess
        @Nullable
        public Chunk getChunk(@Nullable IBlockAccess worldIn, @Nonnull BlockPos pos) {
            if(worldIn instanceof World) return ((World)worldIn).getChunkFromBlockCoords(pos);
            return (worldIn instanceof IChunkProvider) ? ((IChunkProvider)worldIn).getChunkFromBlockCoords(pos) : null;
        }
    }

    //handles client-side code
    @SuppressWarnings("unused")
    public static class ClientProxy extends CommonProxy
    {
        //prioritize the Minecraft#world instance, with a fallback to the input
        @Nullable
        @Override
        public Chunk getChunk(@Nullable IBlockAccess worldIn, @Nonnull BlockPos pos) {
            final @Nullable World world = Minecraft.getMinecraft().world;
            return super.getChunk(world != null ? world : worldIn, pos);
        }
    }
}
