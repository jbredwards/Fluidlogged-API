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
                .put("net.minecraft.client.multiplayer.WorldClient", new WorldClientPlugin()) //non-empty FluidStates call randomDisplayTick
                .put("net.minecraft.client.particle.ParticleSuspend", new ParticleSuspendPlugin()) //this particle doesn't instantly disappear while inside water FluidStates
                .put("net.minecraft.client.renderer.chunk.RenderChunk", new RenderChunkPlugin()) //allows the game to render FluidStates
                .put("net.minecraft.client.renderer.ActiveRenderInfo", new ActiveRenderInfoPlugin()) //fixes FluidState fog color & fov
                .put("net.minecraft.client.renderer.EntityRenderer", new EntityRendererPlugin()) //fixes graphical underwater block selection; lava FluidStates now emit smoke while raining; fixes FluidState fog color
                //vanilla (blocks)
                .put("net.minecraft.block.Block", new BlockPlugin()) //fixes some lighting, canSustainPlant, and explosion related issues
                .put("net.minecraft.block.BlockBush", new BlockBushPlugin()) //breaking this block type no longer voids the possible FluidState here
                .put("net.minecraft.block.BlockCocoa", new BlockCocoaPlugin()) //breaking this block type no longer voids the possible FluidState here
                .put("net.minecraft.block.BlockConcretePowder", new BlockConcretePowderPlugin()) //concrete forms from concrete powder while it's next to water FluidStates
                .put("net.minecraft.block.BlockDynamicLiquid", new BlockDynamicLiquidPlugin()) //vanilla fluids no longer do block mixing when they shouldn't; vanilla fluids now flow from fluidlogged blocks
                .put("net.minecraft.block.BlockFarmland", new BlockFarmlandPlugin()) //farmland blocks now recognise water FluidStates
                .put("net.minecraft.block.BlockLilyPad", new BlockLilyPadPlugin()) //lily pads can stay on certain water FluidStates
                .put("net.minecraft.block.BlockLiquid", new BlockLiquidPlugin()) //fixes BlockLiquid#getBlockLiquidHeight
                .put("net.minecraft.block.BlockReed", new BlockReedPlugin()) //sugar cane blocks now recognise water FluidStates
                .put("net.minecraft.block.BlockSkull", new BlockSkullPlugin()) //wither skulls no longer void the FluidState here when summoning the wither
                .put("net.minecraft.block.BlockSponge", new BlockSpongePlugin()) //fixes drain interactions across all modded fluids & FluidStates
                //vanilla (entities)
                .put("net.minecraft.entity.ai.EntityAIPanic", new EntityAIPanicPlugin()) //water FluidStates are now seen as water blocks
                .put("net.minecraft.entity.ai.RandomPositionGenerator", new RandomPositionGeneratorPlugin()) //water FluidStates are now seen as water blocks
                .put("net.minecraft.entity.item.EntityBoat", new EntityBoatPlugin()) //boat work with water FluidStates
                .put("net.minecraft.entity.item.EntityItem", new EntityItemPlugin()) //items generate the burn effects when in a lava FluidState
                .put("net.minecraft.entity.item.EntityXPOrb", new EntityItemPlugin()) //xp orbs generate the burn effects when in a lava FluidState
                .put("net.minecraft.entity.projectile.EntityFishHook", new EntityFishHookPlugin()) //fishhook entities generate the fishing particles at water FluidStates
                .put("net.minecraft.entity.Entity", new EntityPlugin()) //isInsideOfMaterial now recognises FluidStates
                //vanilla (world)
                .put("net.minecraft.world.chunk.Chunk", new ChunkPlugin()) //WIP
                .put("net.minecraft.world.end.DragonSpawnManager$3", new DragonSpawnManagerPlugin()) //summoning the ender dragon will now void FluidStates at the pillar locations
                .put("net.minecraft.world.gen.feature.WorldGenDungeons", new WorldGenDungeonsPlugin()) //spawner dungeons now void FluidStates when they generate
                .put("net.minecraft.world.World", new WorldPlugin()) //corrects a lot of FluidState related interactions
                .put("net.minecraft.world.WorldServer", new WorldServerPlugin()) //FluidStates now get ticked
                //forge
                .put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new ModelFluidPlugin()) //fixes all issues with fluidlogged z-fighting
                .put("net.minecraftforge.common.ForgeHooks", new ForgeHooksPlugin()) //fix ForgeHooks#isInsideOfMaterial by allowing it to access stored fluid blocks
                .put("net.minecraftforge.fluids.BlockFluidBase", new BlockFluidBasePlugin()) //WIP
                .put("net.minecraftforge.fluids.BlockFluidClassic", new BlockFluidClassicPlugin()) //WIP
                .put("net.minecraftforge.fluids.FluidUtil", new FluidUtilPlugin()) //changes some of this class's util functions to be FluidState sensitive
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
