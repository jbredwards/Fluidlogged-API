package git.jbredwards.fluidlogged;

import com.google.common.collect.ImmutableList;
import git.jbredwards.fluidlogged.client.util.FluidloggedBlockRendererDispatcher;
import git.jbredwards.fluidlogged.client.util.FluidloggedTESR;
import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import net.minecraft.block.material.Material;
import net.minecraft.client.Minecraft;
import net.minecraft.init.Biomes;
import net.minecraft.world.biome.BiomeColorHelper;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.common.*;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import static git.jbredwards.fluidlogged.util.FluidloggedConstants.*;

/**
 *
 * @author jbred
 *
 */
@Mod(modid = MODID, name = NAME, version = VERSION)
public final class Fluidlogged extends DummyModContainer
{
    //vanilla fluidlogged te's, the modded ones get registered automatically through FluidPlugin
    public static final BlockFluidloggedTE WATERLOGGED_TE = new BlockFluidloggedTE(FluidRegistry.WATER, Material.WATER);
    public static final BlockFluidloggedTE LAVALOGGED_TE = new BlockFluidloggedTE(FluidRegistry.LAVA, Material.LAVA);
    static {
        FLUIDLOGGED_TE_LOOKUP.put(FluidRegistry.WATER, WATERLOGGED_TE);
        FLUIDLOGGED_TE_LOOKUP.put(FluidRegistry.LAVA, LAVALOGGED_TE);
    }

    //plugin
    public Fluidlogged() {
        super(new ModMetadata());
        ModMetadata meta = getMetadata();
        meta.modId = MODID + "_plugin";
        meta.name = NAME + " Plugin";
        meta.version = VERSION;
        meta.credits = "jbredwards";
        meta.authorList = ImmutableList.of("jbredwards");
        meta.url = "https://curseforge.com/minecraft/mc-mods/fluidlogged-api";
    }

    @SuppressWarnings("unused")
    @Mod.EventHandler
    public static void init(FMLInitializationEvent event) {
        if(event.getSide() == Side.CLIENT) clientInit();
    }

    @SideOnly(Side.CLIENT)
    private static void clientInit() {
        //changes the vanilla block renderer
        ObfuscationReflectionHelper.setPrivateValue(Minecraft.class, Minecraft.getMinecraft(), new FluidloggedBlockRendererDispatcher(Minecraft.getMinecraft().getBlockRendererDispatcher()), "field_175618_aM");
        //tile entity special renderer (does nothing by default, only for IFluidloggable blocks that choose to use it)
        ClientRegistry.bindTileEntitySpecialRenderer(TileEntityFluidlogged.class, new FluidloggedTESR());
        //colors the waterlogged te
        Minecraft.getMinecraft().getBlockColors().registerBlockColorHandler((state, world, pos, index) -> {
            if(pos == null || world == null) return Biomes.PLAINS.getWaterColor();
            else return BiomeColorHelper.getWaterColorAtPos(world, pos);
        }, WATERLOGGED_TE);
    }
}
