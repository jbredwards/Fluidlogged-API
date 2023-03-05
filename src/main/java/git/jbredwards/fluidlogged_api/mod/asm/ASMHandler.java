package git.jbredwards.fluidlogged_api.mod.asm;

import git.jbredwards.fluidlogged_api.api.asm.AbstractClassTransformer;
import git.jbredwards.fluidlogged_api.api.asm.BasicLoadingPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.*;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigHandler;

import javax.annotation.Nonnull;
import java.util.Map;

/**
 * handler for only this mod's plugins
 * @author jbred
 *
 */
@BasicLoadingPlugin.SortingIndex(1401)
@BasicLoadingPlugin.Name("Fluidlogged API Plugin")
@BasicLoadingPlugin.MCVersion("1.12.2")
public final class ASMHandler implements BasicLoadingPlugin
{
    //this class exists cause the vanilla launcher needs the transformer & plugin to be different classes for reasons?
    public static final class Transformer extends AbstractClassTransformer
    {
        public Transformer() {
            //forge
            plugins.put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new PluginModelFluid()); //fixes all issues with fluidlogged z-fighting
            plugins.put("net.minecraftforge.common.ForgeHooks", new PluginForgeHooks()); //fix ForgeHooks#isInsideOfMaterial by allowing it to access stored fluid blocks
            plugins.put("net.minecraftforge.fluids.BlockFluidBase", new PluginBlockFluidBase()); //modded fluids work properly with the mod & prevent startup crash
            plugins.put("net.minecraftforge.fluids.BlockFluidClassic", new PluginBlockFluidClassic()); //modded fluids work properly with the mod
            plugins.put("net.minecraftforge.fluids.Fluid", new PluginFluid()); //store one internal FluidState for each Fluid, as to decrease ram usage
            plugins.put("net.minecraftforge.fluids.FluidRegistry$1", new PluginFluidWater()); //add water's biome colors to its fluid class
            plugins.put("net.minecraftforge.fluids.FluidUtil", new PluginFluidUtil()); //changes some of this class's util functions to be FluidState sensitive
            //modded
            plugins.put("biomesoplenty.common.fluids.blocks.BlockBloodFluid", new PluginBiomesOPlenty()); //fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockHotSpringWaterFluid", new PluginBiomesOPlenty()); //fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockPoisonFluid", new PluginBiomesOPlenty()); //fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockQuicksandFluid", new PluginBiomesOPlenty()); //fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.item.ItemBOPLilypad", new PluginItemLilyPad()); //lily pads can be placed on certain water FluidStates
            plugins.put("codechicken.multipart.BlockMultipart", new PluginFluidloggableBlocks()); //make multipart blocks fluidloggable by default
            plugins.put("codechicken.multipart.ItemPlacementHelper$", new PluginCBMultipart()); //allow multipart blocks to be placed in fluids
            plugins.put("codechicken.multipart.TileMultipart", new PluginCBMultipart()); //prevent desync when removing a fluidlogged multipart block
            plugins.put("cofh.thermaldynamics.block.BlockTDBase", new PluginThermalDynamics()); //fix fluidlogged duct explosion resistance
            plugins.put("cofh.thermaldynamics.duct.tiles.TileGrid", new PluginThermalDynamics()); //ray trace now skips fluids
            plugins.put("crafttweaker.mc1120.block.MCWorldBlock", new PluginCraftTweaker()); //MCWorldBlock.getFluid can read FluidStates
            plugins.put("dev.necauqua.mods.cm.mixin.entity.EntityMixin", new PluginChiseledMe()); //fix chiseled me conflict, fluidlogged api already implements these patches
            plugins.put("dev.necauqua.mods.cm.mixin.WorldMixin", new PluginChiseledMe()); //fix chiseled me conflict, fluidlogged api already implements these patches
            plugins.put("exnihilocreatio.barrel.modes.fluid.BarrelModeFluid", new PluginExNihiloCreatio()); //allow "fluid on top" barrel crafting to accept FluidStates
            plugins.put("hellfirepvp.astralsorcery.common.block.fluid.FluidBlockLiquidStarlight", new PluginAstralSorcery()); //fixes weird mixing interactions
            plugins.put("io.github.lxgaming.sledgehammer.mixin.core.block.BlockDynamicLiquidMixin", new PluginSledgehammer()); //remove redundant transformer
            plugins.put("io.github.opencubicchunks.cubicchunks.core.asm.mixin.core.client.MixinChunkCache_HeightLimits", new PluginCubicChunks()); //fix mixin annotation to target fluidlogged api transform
            plugins.put("me.jellysquid.mods.phosphor.mod.world.lighting.LightingEngine", new PluginHesperus()); //phosphor takes FluidStates into account when computing light
            plugins.put("me.jellysquid.mods.phosphor.mod.world.lighting.LightingHooks", new PluginHesperus()); //phosphor takes FluidStates into account when computing light
            plugins.put("meldexun.nothirium.mc.renderer.chunk.RenderChunk$ChunkCache", new PluginNothirium()); //better nothirium compat (for old versions)
            plugins.put("meldexun.nothirium.mc.renderer.chunk.SectionRenderCache", new PluginNothirium()); //better nothirium compat (for new versions)
            plugins.put("mods.railcraft.common.fluids.CustomContainerHandler", new PluginRailcraft()); //fix railcraft uncraftable potion bug when collecting water bottles (issue#148)
            plugins.put("mrtjp.projectred.core.TFaceConnectable$class", new PluginProjectRed()); //allow wires to connect through fluids
            plugins.put("net.optifine.override.ChunkCacheOF", new PluginOptifine()); //better optifine compat
            plugins.put("org.spongepowered.common.mixin.core.block.BlockDynamicLiquidMixin", new PluginSpongeForge()); //spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.block.BlockLiquidMixin", new PluginSpongeForge()); //spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.block.BlockStaticLiquidMixin", new PluginSpongeForge()); //spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.entity.EntityMixin", new PluginSpongeForge()); //spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.optimization.world.chunk.ChunkMixin_Async_Lighting", new PluginSpongeForge()); //spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.mod.mixin.core.forge.fluids.BlockFluidClassicMixin_Forge", new PluginSpongeForge()); //spongeforge no longer mixins into conflicting methods
            plugins.put("plus.misterplus.plustweaks.mixins.MixinBlockFluidBase", new PluginPlusTweaks()); //fix crash with PlusTweaks mod fluid interactions
            plugins.put("plus.misterplus.plustweaks.mixins.MixinBlockLiquid", new PluginPlusTweaks()); //fix crash with PlusTweaks mod fluid interactions
            plugins.put("portablejim.bbw.core.WandWorker", new PluginBuildersWands()); //better builders wands compat
            plugins.put("thebetweenlands.common.block.plant.BlockAlgae", new PluginBlockLilyPad()); //lily pads can stay on certain water FluidStates
            plugins.put("thebetweenlands.common.block.terrain.BlockSwampWater", new PluginBetweenlands()); //betweenlands compat
            plugins.put("thebetweenlands.common.item.ItemWaterPlaceable", new PluginItemLilyPad()); //lily pads can be placed on certain water FluidStates
            plugins.put("twilightforest.block.BlockTFHugeLilyPad", new PluginBlockLilyPad()); //lily pads can stay on certain water FluidStates
            plugins.put("twilightforest.item.ItemBlockTFHugeLilyPad", new PluginTwilightForest()); //2x2 lily pads can be placed on certain water FluidStates
            plugins.put("twilightforest.item.ItemBlockTFHugeWaterLily", new PluginItemLilyPad()); //lily pads can be placed on certain water FluidStates
            plugins.put("vazkii.botania.common.world.SkyblockWorldEvents", new PluginGardenOfGlass()); //wooden bowls can now be filled by using water FluidStates
            //vanilla (blocks)
            plugins.put("net.minecraft.block.material.MaterialLogic", new PluginMaterialLogic()); //prevents fluids from destroying "circuit" blocks
            plugins.put("net.minecraft.block.Block", new PluginBlock()); //fixes some lighting, canSustainPlant, and explosion related issues
            plugins.put("net.minecraft.block.BlockBarrier", new PluginBlockBarrier()); //move the hardcoded stuff from WorldClient to BlockBarrier
            plugins.put("net.minecraft.block.BlockBush", new PluginBlockBush()); //exists for fluidloggable plants that parent from this class
            plugins.put("net.minecraft.block.BlockCocoa", new PluginBlockCocoa()); //exists in case cocoa beans are added to the config whitelist
            plugins.put("net.minecraft.block.BlockConcretePowder", new PluginBlockConcretePowder()); //concrete forms from concrete powder while its next to flowing water FluidStates
            plugins.put("net.minecraft.block.BlockDoor", new PluginBlockDoor()); //update upper FluidState & correct canFluidFlow
            plugins.put("net.minecraft.block.BlockDynamicLiquid", new PluginBlockDynamicLiquid()); //fixes a bunch of liquid interactions while fluidlogged
            plugins.put("net.minecraft.block.BlockFarmland", new PluginBlockFarmland()); //farmland blocks now recognise water FluidStates
            plugins.put("net.minecraft.block.BlockFire", new PluginBlockFire()); //fire doesn't destroy fluidlogged fluids
            plugins.put("net.minecraft.block.BlockGrass", new PluginBlockGrass()); //use World#getBlockLightOpacity for FluidState sensitivity
            plugins.put("net.minecraft.block.BlockLeaves", new PluginFluidloggableBlocksFlowable()); //for this it makes sense to have the fluid flow from any side
            plugins.put("net.minecraft.block.BlockLilyPad", new PluginBlockLilyPad()); //lily pads can stay on certain water FluidStates
            plugins.put("net.minecraft.block.BlockLiquid", new PluginBlockLiquid()); //significantly changes the BlockLiquid class to work with the mod
            plugins.put("net.minecraft.block.BlockMobSpawner", new PluginFluidloggableBlocksFlowable()); //for this it makes sense to have the fluid flow from any side
            plugins.put("net.minecraft.block.BlockMycelium", new PluginBlockMycelium()); //use World#getBlockLightOpacity for FluidState sensitivity
            plugins.put("net.minecraft.block.BlockPistonBase", new PluginBlockPistonBase()); //piston bases are fluidloggable while extended
            plugins.put("net.minecraft.block.BlockReed", new PluginBlockReed()); //sugar cane blocks now recognise water FluidStates
            plugins.put("net.minecraft.block.BlockSkull", new PluginBlockSkull()); //wither skulls no longer void the FluidState here when summoning the wither
            plugins.put("net.minecraft.block.BlockSlab", new PluginBlockSlab()); //half slabs are fluidloggable
            plugins.put("net.minecraft.block.BlockSponge", new PluginBlockSponge()); //fixes drain interactions across all modded fluids & FluidStates
            plugins.put("net.minecraft.block.BlockStairs", new PluginBlockStairs()); //update neighboring fluids when this changes shape
            plugins.put("net.minecraft.block.BlockStaticLiquid", new PluginBlockStaticLiquid()); //update FluidStates
            plugins.put("net.minecraft.block.BlockTrapDoor", new PluginBlockTrapDoor()); //makes trap doors fluidloggable by default
            plugins.put("net.minecraft.block.BlockTorch", new PluginBlockTorch()); //allow torches to be destroyed by flowing fluid blocks
            plugins.put("net.minecraft.block.BlockWall", new PluginBlockWall()); //fixes a bug with walls that caused the post to unintentionally render
            //vanilla (basic fluidlogging implementation for certain blocks)
            plugins.put("net.minecraft.block.BlockAnvil", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockBanner", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockBasePressurePlate", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockBeacon", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockBrewingStand", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockButton", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockChest", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockDaylightDetector", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockDragonEgg", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockEnderChest", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockEndPortalFrame", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockEnchantmentTable", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockEndRod", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockFenceGate", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockFence", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockFlowerPot", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockHopper", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockLadder", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockLever", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockPane", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockPistonExtension", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockRailBase", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockRedstoneDiode", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockRedstoneTorch", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockRedstoneWire", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockSign", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockTripWire", new PluginFluidloggableBlocks());
            plugins.put("net.minecraft.block.BlockTripWireHook", new PluginFluidloggableBlocks());
            //vanilla (client)
            plugins.put("net.minecraft.client.multiplayer.WorldClient", new PluginWorldClient()); //non-empty FluidStates call randomDisplayTick & move hardcoded barrier stuff to barrier.randomDisplayTick
            plugins.put("net.minecraft.client.particle.ParticleBubble", new PluginWaterParticles()); //this doesn't instantly disappear while inside water FluidStates
            plugins.put("net.minecraft.client.particle.ParticleDrip", new PluginWaterParticles()); //this doesn't instantly disappear while inside water FluidStates
            plugins.put("net.minecraft.client.particle.ParticleRain", new PluginParticleRain()); //fix all fluid-related rain collisions
            plugins.put("net.minecraft.client.particle.ParticleSuspend", new PluginWaterParticles()); //this doesn't instantly disappear while inside water FluidStates
            plugins.put("net.minecraft.client.renderer.chunk.RenderChunk", new PluginRenderChunk()); //allows the game to render FluidStates
            plugins.put("net.minecraft.client.renderer.ActiveRenderInfo", new PluginActiveRenderInfo()); //get block fog color from possible FluidState
            plugins.put("net.minecraft.client.renderer.EntityRenderer", new PluginEntityRenderer()); //fixes graphical underwater block selection; lava FluidStates now emit smoke while raining; fixes FluidState fog color
            //vanilla (entity)
            plugins.put("net.minecraft.entity.ai.EntityAIPanic", new PluginEntityAIPanic()); //water FluidStates are now seen as water blocks
            plugins.put("net.minecraft.entity.ai.RandomPositionGenerator", new PluginRandomPositionGenerator()); //water FluidStates are now seen as water blocks
            plugins.put("net.minecraft.entity.item.EntityBoat", new PluginEntityBoat()); //boats work with water FluidStates
            plugins.put("net.minecraft.entity.item.EntityItem", new PluginEntityItem()); //handle lava collisions correctly
            plugins.put("net.minecraft.entity.item.EntityXPOrb", new PluginEntityItem()); //handle lava collisions correctly
            plugins.put("net.minecraft.entity.projectile.EntityFishHook", new PluginEntityFishHook()); //fishhook entities generate the fishing particles at water FluidStates
            plugins.put("net.minecraft.entity.Entity", new PluginEntity());
            plugins.put("net.minecraft.entity.EntityLivingBase", new PluginEntityLivingBase()); //fix issue#151
            //vanilla (item)
            plugins.put("net.minecraft.item.ItemArmorStand", new PluginItemArmorStand()); //armor stands don't remove fluids at their position when placed
            plugins.put("net.minecraft.item.ItemGlassBottle", new PluginItemGlassBottle()); //glass bottles can now be filled by using water FluidStates
            plugins.put("net.minecraft.item.ItemLilyPad", new PluginItemLilyPad()); //lily pads can be placed on certain water FluidStates
            //vanilla (world)
            plugins.put("net.minecraft.world.chunk.storage.ExtendedBlockStorage", new PluginExtendedBlockStorage()); //fix MC-80966
            plugins.put("net.minecraft.world.chunk.Chunk", new PluginChunk()); //account for FluidState light opacity & light values
            plugins.put("net.minecraft.world.chunk.ChunkPrimer", new PluginChunkPrimer()); //allow mods to generate FluidStates more optimally during world gen
            plugins.put("net.minecraft.world.end.DragonSpawnManager$3", new PluginDragonSpawnManager()); //summoning the ender dragon will now void FluidStates at the pillar locations
            plugins.put("net.minecraft.world.gen.feature.WorldGenDungeons", new PluginWorldGenDungeons()); //spawner dungeons now void FluidStates when they generate
            plugins.put("net.minecraft.world.gen.structure.template.Template", new PluginTemplate()); //structures can load saved FluidStates
            plugins.put("net.minecraft.world.ChunkCache", new PluginChunkCache()); //implements IChunkProvider
            plugins.put("net.minecraft.world.World", new PluginWorld()); //corrects a lot of FluidState related interactions
            plugins.put("net.minecraft.world.WorldServer", new PluginWorldServer()); //FluidStates now get ticked
        }

        @Nonnull
        @Override
        public String getPluginName() { return "Fluidlogged API Plugin"; }
    }

    @Nonnull
    @Override
    public String[] getASMTransformerClass() {
        return new String[] {"git.jbredwards.fluidlogged_api.mod.asm.transformers.TransformerSmoothWater", getPluginClass()};
    }

    @Override
    public void injectData(@Nonnull Map<String, Object> data) { FluidloggedAPIConfigHandler.init(); }
}
