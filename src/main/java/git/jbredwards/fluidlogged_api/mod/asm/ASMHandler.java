package git.jbredwards.fluidlogged_api.mod.asm;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.mod.Constants;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.*;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.IFMLLoadingPlugin;
import zone.rong.mixinbooter.IEarlyMixinLoader;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;
import java.util.Map;

/**
 * handler for only this mod's plugins
 * @author jbred
 *
 */
@IFMLLoadingPlugin.SortingIndex(1401)
@IFMLLoadingPlugin.Name("Fluidlogged API Plugin")
@IFMLLoadingPlugin.MCVersion("1.12.2")
public final class ASMHandler implements IFMLLoadingPlugin, IEarlyMixinLoader
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
                .put("net.minecraft.world.World", new PluginWorld()) //corrects a lot of FluidState related interactions
                .put("net.minecraft.world.WorldServer", new PluginWorldServer()) //FluidStates now get ticked
                //forge
                .put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new PluginModelFluid()) //fixes all issues with fluidlogged z-fighting
                .put("net.minecraftforge.fluids.BlockFluidBase", new PluginBlockFluidBase()) //prevent startup crash
                .put("net.minecraftforge.fluids.FluidUtil", new PluginFluidUtil()) //changes some of this class's util functions to be FluidState sensitive
                //modded
                .put("thebetweenlands.common.block.terrain.BlockSwampWater", new PluginBetweenlands()) //betweenlands compat
                .build();

        @Override
        public byte[] transform(String name, String transformedName, byte[] basicClass) {
            final @Nullable IASMPlugin plugin = PLUGINS.get(transformedName);
            return plugin == null ? basicClass : plugin.transform(basicClass, obfuscated);
        }
    }

    @Nonnull
    @Override
    public List<String> getMixinConfigs() {
        return ImmutableList.of(
                "META-INF/mixins." + Constants.MODID + ".forge.json",
                "META-INF/mixins." + Constants.MODID + ".vanilla.block.json",
                "META-INF/mixins." + Constants.MODID + ".vanilla.client.json",
                "META-INF/mixins." + Constants.MODID + ".vanilla.entity.json",
                "META-INF/mixins." + Constants.MODID + ".vanilla.world.json"
        );
    }

    @Nonnull
    @Override
    public String[] getASMTransformerClass() {
        return new String[] { "git.jbredwards.fluidlogged_api.mod.asm.ASMHandler$Transformer" };
    }

    @Override
    public void injectData(@Nonnull Map<String, Object> data) {
        obfuscated = (boolean)data.get("runtimeDeobfuscationEnabled");
        ConfigHandler.init();
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
