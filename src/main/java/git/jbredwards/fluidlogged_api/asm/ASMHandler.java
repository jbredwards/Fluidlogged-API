package git.jbredwards.fluidlogged_api.asm;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import git.jbredwards.fluidlogged_api.asm.plugins.forge.*;
import git.jbredwards.fluidlogged_api.asm.plugins.vanilla.block.*;
import git.jbredwards.fluidlogged_api.asm.plugins.vanilla.client.*;
import git.jbredwards.fluidlogged_api.asm.plugins.vanilla.entity.*;
import git.jbredwards.fluidlogged_api.asm.plugins.vanilla.world.*;
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
@IFMLLoadingPlugin.SortingIndex(1401)
@IFMLLoadingPlugin.Name("Fluidlogged API Plugin")
@IFMLLoadingPlugin.MCVersion("1.12.2")
public final class ASMHandler implements IFMLLoadingPlugin
{
    //this class exists cause the vanilla launcher needs the transformer & plugin to be different classes for reasons?
    public static final class Transformer implements IClassTransformer
    {
        //plugin registry
        @Nonnull
        public static Map<String, IASMPlugin> PLUGINS = new ImmutableMap.Builder<String, IASMPlugin>()
                //vanilla (client)
                .put("net.minecraft.client.multiplayer.WorldClient", new WorldClientPlugin())
                .put("net.minecraft.client.particle.ParticleSuspend", new ParticleSuspendPlugin())
                .put("net.minecraft.client.renderer.chunk.RenderChunk", new RenderChunkPlugin())
                .put("net.minecraft.client.renderer.ActiveRenderInfo", new ActiveRenderInfoPlugin())
                .put("net.minecraft.client.renderer.EntityRenderer", new EntityRendererPlugin())
                //vanilla (blocks)
                .put("net.minecraft.block.Block", new BlockPlugin())
                .put("net.minecraft.block.BlockBush", new BlockBushPlugin())
                .put("net.minecraft.block.BlockCocoa", new BlockCocoaPlugin())
                .put("net.minecraft.block.BlockConcretePowder", new BlockConcretePowderPlugin())
                .put("net.minecraft.block.BlockDynamicLiquid", new BlockDynamicLiquidPlugin())
                .put("net.minecraft.block.BlockFarmland", new BlockFarmlandPlugin())
                .put("net.minecraft.block.BlockLilyPad", new BlockLilyPadPlugin())
                .put("net.minecraft.block.BlockReed", new BlockReedPlugin())
                .put("net.minecraft.block.BlockSkull", new BlockSkullPlugin())
                .put("net.minecraft.block.BlockSponge", new BlockSpongePlugin())
                //vanilla (entities)
                .put("net.minecraft.entity.ai.EntityAIPanic", new EntityAIPanicPlugin())
                .put("net.minecraft.entity.ai.RandomPositionGenerator", new RandomPositionGeneratorPlugin())
                .put("net.minecraft.entity.item.EntityItem", new EntityItemPlugin())
                .put("net.minecraft.entity.item.EntityXPOrb", new EntityItemPlugin())
                .put("net.minecraft.entity.projectile.EntityFishHook", new EntityFishHookPlugin())
                .put("net.minecraft.entity.Entity", new EntityPlugin())
                //vanilla (world)
                .put("net.minecraft.world.end.DragonSpawnManager$3", new DragonSpawnManagerPlugin())
                .put("net.minecraft.world.gen.feature.WorldGenDungeons", new WorldGenDungeonsPlugin())
                .put("net.minecraft.world.World", new WorldPlugin())
                .put("net.minecraft.world.WorldServer", new WorldServerPlugin())
                //forge
                .put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new ModelFluidPlugin())
                .put("net.minecraftforge.common.ForgeHooks", new ForgeHooksPlugin())
                .put("net.minecraftforge.fluids.capability.wrappers.BlockLiquidWrapper", new BlockLiquidWrapperPlugin())
                .put("net.minecraftforge.fluids.BlockFluidBase", new BlockFluidBasePlugin())
                .put("net.minecraftforge.fluids.Fluid", new FluidPlugin())
                .put("net.minecraftforge.fluids.FluidUtil", new FluidUtilPlugin())
                .build();

        @Override
        public byte[] transform(String name, String transformedName, byte[] basicClass) {
            final @Nullable IASMPlugin plugin = PLUGINS.get(transformedName);
            return plugin == null ? basicClass : plugin.transform(basicClass, !name.equals(transformedName));
        }
    }

    @Override
    public String[] getASMTransformerClass() {
        return new String[] {
                "git.jbredwards.fluidlogged_api.asm.ASMReplacer",
                "git.jbredwards.fluidlogged_api.asm.ASMHandler$Transformer"
        };
    }

    @Nullable
    @Override
    public String getModContainerClass() { return null; }

    @Nullable
    @Override
    public String getSetupClass() { return null; }

    @Override
    public void injectData(@Nullable Map<String, Object> data) { }

    @Nullable
    @Override
    public String getAccessTransformerClass() { return null; }
}
