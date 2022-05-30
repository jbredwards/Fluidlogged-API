package git.jbredwards.fluidlogged_api.mod.asm;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.*;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
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
                //forge
                .put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new PluginModelFluid()) //fixes all issues with fluidlogged z-fighting
                .put("net.minecraftforge.common.ForgeHooks", new PluginForgeHooks()) //fix ForgeHooks#isInsideOfMaterial by allowing it to access stored fluid blocks
                .put("net.minecraftforge.fluids.BlockFluidBase", new PluginBlockFluidBase()) //modded fluids work properly with the mod & prevent startup crash
                .put("net.minecraftforge.fluids.BlockFluidClassic", new PluginBlockFluidClassic()) //modded fluids work properly with the mod
                .put("net.minecraftforge.fluids.Fluid", new PluginFluid()) //store one internal FluidState for each Fluid, as to decrease ram usage
                .put("net.minecraftforge.fluids.FluidUtil", new PluginFluidUtil()) //changes some of this class's util functions to be FluidState sensitive
                //modded
                .put("biomesoplenty.common.fluids.blocks.BlockBloodFluid", new PluginBiomesOPlenty()) //fix BOP fluid block mixing
                .put("biomesoplenty.common.fluids.blocks.BlockHotSpringWaterFluid", new PluginBiomesOPlenty()) //fix BOP fluid block mixing
                .put("biomesoplenty.common.fluids.blocks.BlockPoisonFluid", new PluginBiomesOPlenty()) //fix BOP fluid block mixing
                .put("biomesoplenty.common.fluids.blocks.BlockQuicksandFluid", new PluginBiomesOPlenty()) //fix BOP fluid block mixing
                .put("cofh.thermaldynamics.block.BlockTDBase", new PluginThermalDynamics()) //fix fluidlogged duct explosion resistance
                .put("cofh.thermaldynamics.duct.tiles.TileGrid", new PluginThermalDynamics()) //ray trace now skips fluids
                .put("org.spongepowered.common.mixin.core.entity.EntityMixin", new PluginSpongeForge()) //spongeforge no longer mixins into conflicting methods
                .put("thebetweenlands.common.block.terrain.BlockSwampWater", new PluginBetweenlands()) //betweenlands compat
                .put("portablejim.bbw.core.WandWorker", new PluginBuildersWands()) //better builders wands compat
                //vanilla (client)
                .put("net.minecraft.client.multiplayer.WorldClient", new PluginWorldClient()) //non-empty FluidStates call randomDisplayTick & move hardcoded barrier stuff to barrier.randomDisplayTick
                .put("net.minecraft.client.particle.Particle", new PluginParticle()) //fix particle lighting while within fluidlogged blocks
                .put("net.minecraft.client.particle.ParticleBubble", new PluginWaterParticles()) //this doesn't instantly disappear while inside water FluidStates
                .put("net.minecraft.client.particle.ParticleDrip", new PluginWaterParticles()) //this doesn't instantly disappear while inside water FluidStates
                .put("net.minecraft.client.particle.ParticleRain", new PluginParticleRain()) //fix all fluid-related rain collisions
                .put("net.minecraft.client.particle.ParticleSuspend", new PluginWaterParticles()) //this doesn't instantly disappear while inside water FluidStates
                .put("net.minecraft.client.renderer.chunk.RenderChunk", new PluginRenderChunk()) //allows the game to render FluidStates
                .put("net.minecraft.client.renderer.ActiveRenderInfo", new PluginActiveRenderInfo()) //get block fog color from possible FluidState
                .put("net.minecraft.client.renderer.EntityRenderer", new PluginEntityRenderer()) //fixes graphical underwater block selection; lava FluidStates now emit smoke while raining; fixes FluidState fog color
                //vanilla (blocks)
                .put("net.minecraft.block.material.MaterialLogic", new PluginMaterialLogic()) //prevents fluids from destroying "circuit" blocks
                .put("net.minecraft.block.Block", new PluginBlock()) //fixes some lighting, canSustainPlant, and explosion related issues
                .put("net.minecraft.block.BlockBarrier", new PluginBlockBarrier()) //move the hardcoded stuff from WorldClient to BlockBarrier
                .put("net.minecraft.block.BlockBush", new PluginBlockBush()) //exists for fluidloggable plants that parent from this class
                .put("net.minecraft.block.BlockCocoa", new PluginBlockCocoa()) //exists in case cocoa beans are added to the config whitelist
                .put("net.minecraft.block.BlockConcretePowder", new PluginBlockConcretePowder()) //concrete forms from concrete powder while its next to flowing water FluidStates
                .put("net.minecraft.block.BlockDoor", new PluginBlockDoor()) // update upper FluidState & correct canFluidFlow
                .put("net.minecraft.block.BlockDynamicLiquid", new PluginBlockDynamicLiquid()) //fixes a bunch of liquid interactions while fluidlogged
                .put("net.minecraft.block.BlockFarmland", new PluginBlockFarmland()) //farmland blocks now recognise water FluidStates
                .put("net.minecraft.block.BlockLeaves", new PluginFluidloggableBlocksFlowable()) //for this it makes sense to have the fluid flow from any side
                .put("net.minecraft.block.BlockLilyPad", new PluginBlockLilyPad()) //lily pads can stay on certain water FluidStates
                .put("net.minecraft.block.BlockLiquid", new PluginBlockLiquid()) //significantly changes the BlockLiquid class to work with the mod
                .put("net.minecraft.block.BlockMobSpawner", new PluginFluidloggableBlocksFlowable()) //for this it makes sense to have the fluid flow from any side
                .put("net.minecraft.block.BlockPistonBase", new PluginBlockPistonBase()) //piston bases are fluidloggable while extended
                .put("net.minecraft.block.BlockReed", new PluginBlockReed()) //sugar cane blocks now recognise water FluidStates
                .put("net.minecraft.block.BlockSkull", new PluginBlockSkull()) //wither skulls no longer void the FluidState here when summoning the wither
                .put("net.minecraft.block.BlockSlab", new PluginBlockSlab()) //half slabs are fluidloggable
                .put("net.minecraft.block.BlockSponge", new PluginBlockSponge()) //fixes drain interactions across all modded fluids & FluidStates
                .put("net.minecraft.block.BlockTrapDoor", new PluginBlockTrapDoor()) //fluids flow from correct sides
                .put("net.minecraft.block.BlockTorch", new PluginBlockTorch()) //allow torches to be destroyed by flowing fluid blocks
                .put("net.minecraft.block.BlockWall", new PluginBlockWall()) //fixes a bug with walls that caused the post to unintentionally render
                //vanilla (basic fluidlogging implementation for certain blocks)
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
                //vanilla (entity)
                .put("net.minecraft.entity.ai.EntityAIPanic", new PluginEntityAIPanic()) //water FluidStates are now seen as water blocks
                .put("net.minecraft.entity.ai.RandomPositionGenerator", new PluginRandomPositionGenerator()) //water FluidStates are now seen as water blocks
                .put("net.minecraft.entity.item.EntityBoat", new PluginEntityBoat()) //boats work with water FluidStates
                .put("net.minecraft.entity.item.EntityItem", new PluginEntityItem()) //handle lava collisions correctly
                .put("net.minecraft.entity.item.EntityXPOrb", new PluginEntityItem()) //handle lava collisions correctly
                .put("net.minecraft.entity.projectile.EntityFishHook", new PluginEntityFishHook()) //fishhook entities generate the fishing particles at water FluidStates
                .put("net.minecraft.entity.Entity", new PluginEntity())
                //vanilla (world)
                .put("net.minecraft.world.end.DragonSpawnManager$3", new PluginDragonSpawnManager()) //summoning the ender dragon will now void FluidStates at the pillar locations
                .put("net.minecraft.world.gen.feature.WorldGenDungeons", new PluginWorldGenDungeons()) //spawner dungeons now void FluidStates when they generate
                .put("net.minecraft.world.gen.structure.template.Template", new PluginTemplate()) //structures can load saved FluidStates
                .put("net.minecraft.world.ChunkCache", new PluginChunkCache()) //fix lighting bugs
                .put("net.minecraft.world.World", new PluginWorld()) //corrects a lot of FluidState related interactions
                .put("net.minecraft.world.WorldServer", new PluginWorldServer()) //FluidStates now get ticked
                //internal
                .put("git.jbredwards.fluidlogged_api.mod.common.util.AccessorUtils", new PluginAccessorUtils())
                .build();

        @Override
        public byte[] transform(String name, String transformedName, byte[] basicClass) {
            final @Nullable IASMPlugin plugin = PLUGINS.get(transformedName);
            return plugin == null ? basicClass : plugin.transform(basicClass, obfuscated);
        }
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
