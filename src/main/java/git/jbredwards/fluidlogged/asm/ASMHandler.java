package git.jbredwards.fluidlogged.asm;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged.Fluidlogged;
import git.jbredwards.fluidlogged.asm.plugin.*;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.IFMLLoadingPlugin;

import javax.annotation.Nullable;
import java.util.Map;

/**
 * handler for only this mod's plugins
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@IFMLLoadingPlugin.SortingIndex(1001)
@IFMLLoadingPlugin.Name("Fluidlogged Plugin")
@IFMLLoadingPlugin.MCVersion("1.12.2")
@IFMLLoadingPlugin.TransformerExclusions({"git.jbredwards.fluidlogged.asm", "git.jbredwards.fluidlogged.asm.plugin"})
public final class ASMHandler implements IFMLLoadingPlugin
{
    //this class exists cause the vanilla launcher needs the transformer & plugin to be different classes for reasons?
    public static final class Transformer implements IClassTransformer
    {
        //plugin registry
        static final Map<String, AbstractPlugin> PLUGINS = new ImmutableMap.Builder<String, AbstractPlugin>()
                .put("net.minecraft.block.BlockFenceGate", new BlockFenceGatePlugin())
                .put("net.minecraft.block.BlockSponge", new BlockSpongePlugin())
                .put("net.minecraft.block.BlockStairs", new BlockStairsPlugin())
                .put("net.minecraft.block.BlockTrapDoor", new BlockTrapDoorPlugin())
                .put("net.minecraft.client.particle.ParticleDigging", new ParticleDiggingPlugin())
                .put("net.minecraft.client.renderer.BlockFluidRenderer", new BlockFluidRendererPlugin())
                .put("net.minecraft.client.renderer.BlockModelShapes", new BlockModelShapesPlugin())
                .put("net.minecraft.client.renderer.EntityRenderer", new EntityRendererPlugin())
                .put("net.minecraft.client.renderer.chunk.RenderChunk", new RenderChunkPlugin())
                .put("net.minecraftforge.fluids.BlockFluidClassic", new BlockFluidClassicPlugin())
                .put("net.minecraftforge.fluids.Fluid", new FluidPlugin())
                .build();

        @Override
        public byte[] transform(String name, String transformedName, byte[] basicClass) {
            final @Nullable AbstractPlugin plugin = PLUGINS.get(transformedName);
            return plugin == null ? basicClass : plugin.transform(basicClass, !name.equals(transformedName));
        }
    }

    @Override
    public String[] getASMTransformerClass() {
        return new String[] {"git.jbredwards.fluidlogged.asm.ASMHandler$Transformer"};
    }

    @Override
    public String getModContainerClass() {
        return Fluidlogged.class.getName();
    }

    @Nullable
    @Override
    public String getSetupClass() {
        return null;
    }

    @Override
    public void injectData(Map<String, Object> map) { }

    @Override
    public String getAccessTransformerClass() {
        return null;
    }
}
