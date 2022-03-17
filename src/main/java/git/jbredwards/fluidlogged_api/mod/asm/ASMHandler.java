package git.jbredwards.fluidlogged_api.mod.asm;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.mod.Constants;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.*;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.IFMLLoadingPlugin;
import org.spongepowered.asm.launch.MixinBootstrap;
import org.spongepowered.asm.mixin.Mixins;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;

/**
 * handler for only this mod's plugins
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@IFMLLoadingPlugin.SortingIndex(1401)
@IFMLLoadingPlugin.Name("Fluidlogged API Plugin")
@IFMLLoadingPlugin.MCVersion("1.12.2")
public final class ASMHandler implements IFMLLoadingPlugin
{
    private static boolean obfuscated;

    //this class exists cause the vanilla launcher needs the transformer & plugin to be different classes for reasons?
    public static final class Transformer implements IClassTransformer
    {
        //plugin registry
        @Nonnull
        public static Map<String, IASMPlugin> PLUGINS = new ImmutableMap.Builder<String, IASMPlugin>()
                //vanilla (client)
                .put("net.minecraft.client.renderer.chunk.RenderChunk", new PluginRenderChunk()) //allows the game to render FluidStates
                .put("net.minecraft.client.renderer.EntityRenderer", new PluginEntityRenderer()) //fixes graphical underwater block selection; lava FluidStates now emit smoke while raining; fixes FluidState fog color
                //vanilla (blocks)
                .put("net.minecraft.block.Block", new PluginBlock()) //fixes some lighting, canSustainPlant, and explosion related issues
                .put("net.minecraft.block.BlockLilyPad", new PluginBlockLilyPad()) //lily pads can stay on certain water FluidStates
                //vanilla (world)
                .put("net.minecraft.world.gen.feature.WorldGenDungeons", new PluginWorldGenDungeons()) //spawner dungeons now void FluidStates when they generate
                .put("net.minecraft.world.World", new PluginWorld()) //corrects a lot of FluidState related interactions
                .put("net.minecraft.world.WorldServer", new PluginWorldServer()) //FluidStates now get ticked
                //forge
                .put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new PluginModelFluid()) //fixes all issues with fluidlogged z-fighting
                .put("net.minecraftforge.fluids.BlockFluidBase", new PluginBlockFluidBase()) //prevent startup crash
                .put("net.minecraftforge.fluids.FluidUtil", new PluginFluidUtil()) //changes some of this class's util functions to be FluidState sensitive
                .build();

        @Override
        public byte[] transform(String name, String transformedName, byte[] basicClass) {
            final @Nullable IASMPlugin plugin = PLUGINS.get(transformedName);
            return plugin == null ? basicClass : plugin.transform(basicClass, obfuscated);
        }
    }

    @Override
    public String[] getASMTransformerClass() {
        return new String[] { "git.jbredwards.fluidlogged_api.mod.asm.ASMHandler$Transformer" };
    }

    @Override
    public void injectData(@Nonnull Map<String, Object> data) {
        obfuscated = (boolean)data.get("runtimeDeobfuscationEnabled");
        ConfigHandler.init();

        //handle mixins
        MixinBootstrap.init();
        Mixins.addConfiguration("mixins." + Constants.MODID + ".vanilla.block.json");
        Mixins.addConfiguration("mixins." + Constants.MODID + ".vanilla.client.json");
        Mixins.addConfiguration("mixins." + Constants.MODID + ".vanilla.entity.json");
        Mixins.addConfiguration("mixins." + Constants.MODID + ".vanilla.world.json");
        Mixins.addConfiguration("mixins." + Constants.MODID + ".forge.json");
    }

    @Nullable
    @Override
    public String getModContainerClass() { return null; }

    @Nullable
    @Override
    public String getSetupClass() { return null; }

    @Nullable
    @Override
    public String getAccessTransformerClass() { return null; }
}
