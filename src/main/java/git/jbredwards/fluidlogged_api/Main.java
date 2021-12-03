package git.jbredwards.fluidlogged_api;

import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.common.network.NetworkHandler;
import git.jbredwards.fluidlogged_api.common.util.IChunkProvider;
import net.minecraft.block.BlockDispenser;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.init.Items;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.ChunkCache;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.fluids.DispenseFluidContainer;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.SidedProxy;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

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

    //register this mod's capability & packet
    @SuppressWarnings("unused")
    @Mod.EventHandler
    public static void preInit(FMLPreInitializationEvent event) {
        CapabilityManager.INSTANCE.register(IFluidStateCapability.class, IFluidStateCapability.Storage.INSTANCE, IFluidStateCapability.Impl::new);
        NetworkHandler.init();
    }

    //fixes the vanilla bucket dispenser actions by using the forge one instead
    @SuppressWarnings("unused")
    @Mod.EventHandler
    public static void init(FMLInitializationEvent event) {
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.WATER_BUCKET, DispenseFluidContainer.getInstance());
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.LAVA_BUCKET,  DispenseFluidContainer.getInstance());
    }

    //handles client-side code reliably without a try/catch
    public static class CommonProxy
    {
        //tries to get the chunk stored in IBlockAccess at the given pos
        @Nullable
        public Chunk getChunk(@Nullable IBlockAccess world, @Nonnull BlockPos pos) {
            if(world instanceof World) return ((World)world).getChunkFromBlockCoords(pos);
            else if(world instanceof ChunkCache) return ((ChunkCache)world).world.getChunkFromBlockCoords(pos);
            else if(world instanceof IChunkProvider) return ((IChunkProvider)world).getChunkFromBlockCoords(pos);
            else return null;
        }
    }

    //handles client-side code reliably without a try/catch
    @SuppressWarnings("unused")
    public static class ClientProxy extends CommonProxy
    {
        //use WorldClient instance & ignore world input if clientside
        @Nullable
        @Override
        public Chunk getChunk(@Nullable IBlockAccess worldIn, @Nonnull BlockPos pos) {
            final WorldClient world = Minecraft.getMinecraft().world;
            return world == null ? null : world.getChunkFromBlockCoords(pos);
        }
    }
}
