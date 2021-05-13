package git.jbredwards.fluidlogged;

import git.jbredwards.fluidlogged.client.util.FluidloggedBlockRendererDispatcher;
import git.jbredwards.fluidlogged.client.util.FluidloggedTESR;
import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import git.jbredwards.fluidlogged.common.event.FluidloggedEvents;
import git.jbredwards.fluidlogged.util.FluidloggedConstants;
import net.minecraft.block.material.Material;
import net.minecraft.client.Minecraft;
import net.minecraft.world.biome.BiomeColorHelper;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mod.EventBusSubscriber
@Mod(modid = FluidloggedConstants.MODID, name = FluidloggedConstants.NAME, version = FluidloggedConstants.VERSION)
public final class Fluidlogged
{
    //vanilla fluidlogged te's, the modded ones get registered automatically through FluidPlugin
    public static final BlockFluidloggedTE WATERLOGGED_TE = (BlockFluidloggedTE)(new BlockFluidloggedTE(FluidRegistry.WATER, Material.WATER).setRegistryName("waterlogged_te").setUnlocalizedName("water"));
    static { FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.put(FluidRegistry.WATER, WATERLOGGED_TE); }
    public static final BlockFluidloggedTE LAVALOGGED_TE = (BlockFluidloggedTE)(new BlockFluidloggedTE(FluidRegistry.LAVA, Material.LAVA).setRegistryName("lavalogged_te").setUnlocalizedName("lava"));
    static { FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.put(FluidRegistry.LAVA, LAVALOGGED_TE); }

    //non-EventHandler events
    static {
        MinecraftForge.EVENT_BUS.register(new FluidloggedEvents());
    }

    @Mod.EventHandler
    public static void init(FMLInitializationEvent event) {
        if(event.getSide() == Side.CLIENT) clientInit();
    }

    @SideOnly(Side.CLIENT)
    private static void clientInit() {
        //changes the vanilla block renderer
        ObfuscationReflectionHelper.setPrivateValue(Minecraft.class, Minecraft.getMinecraft(), new FluidloggedBlockRendererDispatcher(Minecraft.getMinecraft().getBlockRendererDispatcher()), "field_175618_aM");
        //tile entity special renderer
        ClientRegistry.bindTileEntitySpecialRenderer(TileEntityFluidlogged.class, new FluidloggedTESR());
        //colors the waterlogged te
        Minecraft.getMinecraft().getBlockColors().registerBlockColorHandler((state, world, pos, index) -> BiomeColorHelper.getWaterColorAtPos(world, pos), WATERLOGGED_TE);
    }
}
