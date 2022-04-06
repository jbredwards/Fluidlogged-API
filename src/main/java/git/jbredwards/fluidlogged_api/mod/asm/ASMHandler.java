package git.jbredwards.fluidlogged_api.mod.asm;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
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
                //forge
                .put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new PluginModelFluid()) //fixes all issues with fluidlogged z-fighting
                .put("net.minecraftforge.fluids.BlockFluidBase", new PluginBlockFluidBase()) //prevent startup crash
                .put("net.minecraftforge.fluids.FluidUtil", new PluginFluidUtil()) //changes some of this class's util functions to be FluidState sensitive
                //modded
                .put("thebetweenlands.common.block.terrain.BlockSwampWater", new PluginBetweenlands()) //betweenlands compat
                //vanilla (client)
                .put("net.minecraft.client.renderer.chunk.RenderChunk", new PluginRenderChunk()) //allows the game to render FluidStates
                .put("net.minecraft.client.renderer.EntityRenderer", new PluginEntityRenderer()) //fixes graphical underwater block selection; lava FluidStates now emit smoke while raining; fixes FluidState fog color
                //vanilla (blocks)
                .put("net.minecraft.block.Block", new PluginBlock()) //fixes some lighting, canSustainPlant, and explosion related issues
                .put("net.minecraft.block.BlockLilyPad", new PluginBlockLilyPad()) //lily pads can stay on certain water FluidStates
                .put("net.minecraft.block.BlockTrapDoor", new PluginBlockTrapDoor()) //fluids flow from correct sides
                .put("net.minecraft.block.BlockWall", new PluginBlockWall()) //fixes a bug with walls that caused the post to unintentionally render
                //vanilla (default fluidloggables)
                .put("net.minecraft.block.BlockAnvil", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockBanner", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockBasePressurePlate", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockBeacon", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockBrewingStand", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockButton", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockChest", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockDaylightDetector", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockDragonEgg", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockEnderChest", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockEndPortalFrame", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockEnchantmentTable", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockEndRod", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockFenceGate", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockFence", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockFlowerPot", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockHopper", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockLadder", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockLever", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockPane", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockPistonExtension", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockRailBase", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockRedstoneDiode", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockRedstoneTorch", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockRedstoneWire", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockSign", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockStairs", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockTripWire", new PluginFluidloggableBlocks())
                .put("net.minecraft.block.BlockTripWireHook", new PluginFluidloggableBlocks())
                //vanilla (world)
                .put("net.minecraft.world.World", new PluginWorld()) //corrects a lot of FluidState related interactions
                .put("net.minecraft.world.WorldServer", new PluginWorldServer()) //FluidStates now get ticked
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
                "mixins/fluidlogged_api.forge.json",
                "mixins/fluidlogged_api.vanilla.block.json",
                "mixins/fluidlogged_api.vanilla.client.json",
                "mixins/fluidlogged_api.vanilla.entity.json",
                "mixins/fluidlogged_api.vanilla.world.json"
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
