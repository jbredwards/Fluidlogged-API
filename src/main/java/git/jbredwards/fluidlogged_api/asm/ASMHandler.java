package git.jbredwards.fluidlogged_api.asm;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.asm.plugin.forge.*;
import git.jbredwards.fluidlogged_api.asm.plugin.modded.*;
import git.jbredwards.fluidlogged_api.asm.plugin.vanilla.*;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.IFMLLoadingPlugin;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;

/**
 * handler for only this mod's plugins
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@IFMLLoadingPlugin.SortingIndex(1005)
@IFMLLoadingPlugin.Name("Fluidlogged Plugin")
@IFMLLoadingPlugin.MCVersion("1.12.2")
public class ASMHandler implements IFMLLoadingPlugin
{
    //this class exists cause the vanilla launcher needs the transformer & plugin to be different classes for reasons?
    public static class Transformer implements IClassTransformer
    {
        //plugin registry
        @Nonnull public static Map<String, AbstractPlugin> PLUGINS = new ImmutableMap.Builder<String, AbstractPlugin>()
                //vanilla
                .put("net.minecraft.block.BlockDynamicLiquid", new BlockDynamicLiquidPlugin())
                .put("net.minecraft.block.BlockFenceGate", new BlockFenceGatePlugin())
                .put("net.minecraft.block.BlockSponge", new BlockSpongePlugin())
                .put("net.minecraft.block.BlockStairs", new BlockStairsPlugin())
                .put("net.minecraft.block.BlockTrapDoor", new BlockTrapDoorPlugin())
                .put("net.minecraft.client.particle.ParticleDigging", new ParticleDiggingPlugin())
                .put("net.minecraft.client.renderer.EntityRenderer", new EntityRendererPlugin())
                .put("net.minecraft.client.renderer.chunk.RenderChunk", new RenderChunkPlugin())
                //forge
                .put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new ModelFluidPlugin())
                .put("net.minecraftforge.fluids.BlockFluidBase", new BlockFluidBasePlugin())
                .put("net.minecraftforge.fluids.BlockFluidClassic", new BlockFluidClassicPlugin())
                .put("net.minecraftforge.fluids.Fluid", new FluidPlugin())
                .put("net.minecraftforge.fluids.FluidUtil", new FluidUtilPlugin())
                .put("net.minecraftforge.fluids.capability.wrappers.BlockLiquidWrapper", new BlockLiquidWrapperPlugin())
                .put("net.minecraftforge.fluids.capability.wrappers.FluidBlockWrapper", new FluidBlockWrapperPlugin())
                //modded
                .put("biomesoplenty.common.fluids.blocks.BlockBloodFluid", new BiomesOPlentyPlugin())
                .put("biomesoplenty.common.fluids.blocks.BlockHotSpringWaterFluid", new BiomesOPlentyPlugin())
                .put("biomesoplenty.common.fluids.blocks.BlockQuicksandFluid", new BiomesOPlentyPlugin())
                .put("hellfirepvp.astralsorcery.common.block.fluid.FluidBlockLiquidStarlight", new AstralSorceryPlugin())
                .put("thebetweenlands.common.block.terrain.BlockSwampWater", new BetweenlandsPlugin())
                .put("thebetweenlands.common.registries.FluidRegistry$FluidMultipleBlocks", new FluidPlugin())
                .build();

        @Override
        public byte[] transform(String name, String transformedName, byte[] basicClass) {
            final @Nullable AbstractPlugin plugin = PLUGINS.get(transformedName);
            return plugin == null ? basicClass : plugin.transform(basicClass, !name.equals(transformedName));
        }
    }

    @Override
    public String[] getASMTransformerClass() {
        return new String[] {
                "git.jbredwards.fluidlogged_api.asm.swapper.ASMSwapper",
                "git.jbredwards.fluidlogged_api.asm.ASMHandler$Transformer"
        };
    }

    @Override
    public String getModContainerClass() {
        return null;
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
