/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm;

import git.jbredwards.fluidlogged_api.api.asm.AbstractClassTransformer;
import git.jbredwards.fluidlogged_api.api.asm.BasicLoadingPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.astral_sorcery.PluginAstralSorcery;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands.PluginBetweenlands;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands.PluginBetweenlandsRubber;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands.PluginBetweenlandsStates;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands.PluginBetweenlandsTarBeast;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.biomesoplenty.PluginBiomesOPlenty;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.blue_skies.PluginBlueSkies;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.botania.PluginBotania;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.botania.PluginGardenOfGlass;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.builders_wands.PluginBuildersWands;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cb_multipart.PluginCBMultipart;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.chiseled_me.PluginChiseledMe;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cofhcore.PluginCoFHCore;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.crafttweaker.PluginCraftTweaker;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cubic_chunks.PluginCubicChunks;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.endercore.PluginEnderCore;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.enderio.PluginEnderIO;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.exnihilo.PluginExNihiloCreatio;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.extrautils.PluginExtraUtilsAccessDelegate;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.extrautils.PluginExtraUtilsAccessServer;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft.PluginGalacticraft;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.hesperus.PluginHesperus;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.industrial_renewal.PluginIndustrialRenewal;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs.PluginLycanitesFluidBase;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs.PluginLycanitesFluidMixing;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs.PluginLycanitesFluidSources;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.nethercraft.PluginNethercraftClassic;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.nothirium.PluginNothirium;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.optifine.PluginOptifine;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.plus_tweaks.PluginPlusTweaks;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.projectred.PluginProjectRed;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.railcraft.PluginRailcraft;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.random_things.PluginRandomThings;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.reliquary.PluginReliquary;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.sledgehammer.PluginSledgehammer;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.spongeforge.PluginSpongeForge;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tfc.PluginTFCBlockFluid;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tfc.PluginTFCFluids;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_dynamics.PluginThermalDynamics;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_foundation.PluginThermalAerotheum;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_foundation.PluginThermalFoundation;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_foundation.PluginThermalGlowstone;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tiny_progressions.PluginTinyProgressions;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tropicraft.PluginTropicraftFence;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tropicraft.PluginTropicraftFluid;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tropicraft.PluginTropicraftOverlays;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tropicraft.PluginTropicraftSand;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.twilight_forest.PluginTwilightForest;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.waila.PluginWaila;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.*;

import javax.annotation.Nonnull;

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
    // this class exists cause the vanilla launcher needs the transformer & plugin to be different classes for reasons?
    public static final class Transformer extends AbstractClassTransformer
    {
        public Transformer() {
            // forge
            plugins.put("net.minecraftforge.client.model.ModelFluid", new PluginModelFluid()); // use custom baked model for fluids
            plugins.put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new PluginModelFluid()); // fixes all issues with fluidlogged z-fighting
            plugins.put("net.minecraftforge.common.util.BlockSnapshot", new PluginBlockSnapshot()); // save FluidStates in block snapshots
            plugins.put("net.minecraftforge.common.ForgeHooks", new PluginForgeHooks()); // fix ForgeHooks#isInsideOfMaterial by allowing it to access stored fluid blocks
            plugins.put("net.minecraftforge.fluids.BlockFluidBase", new PluginBlockFluidBase()); // modded fluids work properly with the mod & prevent startup crash
            plugins.put("net.minecraftforge.fluids.BlockFluidClassic", new PluginBlockFluidClassic()); // modded fluids work properly with the mod
            plugins.put("net.minecraftforge.fluids.FluidRegistry$1", new PluginFluidWater()); // add water's biome colors to its fluid class
            plugins.put("net.minecraftforge.fluids.FluidUtil", new PluginFluidUtil()); // changes some of this class's util functions to be FluidState sensitive
            plugins.put("net.minecraftforge.fluids.UniversalBucket", new PluginUniversalBucket()); // only offset the fluid placement pos if the block isn't replaceable or fluidloggable
            // modded
            plugins.put("biomesoplenty.common.block.BlockBOPFarmland", new PluginBlockFarmland()); // farmland blocks now recognise water FluidStates
            plugins.put("biomesoplenty.common.fluids.blocks.BlockBloodFluid", new PluginBiomesOPlenty(false)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockHotSpringWaterFluid", new PluginBiomesOPlenty(false)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockPoisonFluid", new PluginBiomesOPlenty(true)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockQuicksandFluid", new PluginBiomesOPlenty(true)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.item.ItemBOPLilypad", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("cassiokf.industrialrenewal.blocks.abstracts.BlockMultiBlockBase", new PluginIndustrialRenewal()); // Fix industrial renewal mod's multi-blocks
            plugins.put("cassiokf.industrialrenewal.blocks.railroad.BlockCargoLoader", new PluginIndustrialRenewal()); // Fix industrial renewal mod's multi-blocks
            plugins.put("codechicken.multipart.BlockMultipart", new PluginCBMultipart()); // make multipart blocks fluidloggable by default
            plugins.put("codechicken.multipart.ItemPlacementHelper$", new PluginCBMultipart()); // allow multipart blocks to be placed in fluids
            plugins.put("codechicken.multipart.TileMultipart", new PluginCBMultipart()); // prevent desync when removing a fluidlogged multipart block
            plugins.put("cofh.core.fluid.BlockFluidCore", new PluginCoFHCore(false)); // check FluidloggedUtils::canFluidFlow before running fluid block interactions
            plugins.put("cofh.core.fluid.BlockFluidInteractive", new PluginCoFHCore(true)); // use this mod's fluid collision improvements
            plugins.put("cofh.core.proxy.EventHandlerRender", new PluginCoFHCore(false)); // fog and overlay rendering account for FluidStates
            plugins.put("cofh.thermaldynamics.duct.tiles.TileGrid", new PluginThermalDynamics()); // ray trace now skips fluids
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidAerotheum", new PluginThermalAerotheum()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidCryotheum", new PluginThermalFoundation()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidGlowstone", new PluginThermalGlowstone()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidMana", new PluginThermalFoundation()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidPetrotheum", new PluginThermalFoundation()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidPyrotheum", new PluginThermalFoundation()); // fix conflicts
            plugins.put("com.enderio.core.client.handlers.FluidVisualsHandler", new PluginEnderCore()); // make endercore's fluid overlay renderer FluidState-sensitive
            plugins.put("com.enderio.core.common.fluid.BlockFluidEnder", new PluginEnderCore()); // fix endercore fluid collisions
            plugins.put("com.enderio.core.common.util.IBlockAccessWrapper", new PluginEnderCore()); // make endercore's block access wrapper FluidState-sensitive
            plugins.put("com.gildedgames.the_aether.blocks.decorative.BlockAetherWall", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("com.kashdeya.tinyprogressions.items.misc.InfinBucket", new PluginTinyProgressions()); // allow tiny progressions' bucket to recognise tanks and FluidStates
            plugins.put("com.legacy.blue_skies.blocks.natural.BlockSkyFarmland", new PluginBlockFarmland()); // farmland blocks now recognise water FluidStates
            plugins.put("com.legacy.blue_skies.blocks.natural.BlockSkyLilyPad", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("com.legacy.blue_skies.items.ItemSkyBucket", new PluginBlueSkies()); // make blue skies' bucket use its IFluidHandler when placing/taking fluids
            plugins.put("com.legacy.blue_skies.items.ItemSkyBucket$FluidVentiumBucketWrapper", new PluginBlueSkies()); // fix blue skies bucket capability
            plugins.put("com.legacy.blue_skies.items.itemBlocks.ItemSkyLilyPad", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("com.legacy.nethercraft.blocks.natural.BlockNetherFarmland", new PluginBlockFarmland()); // farmland blocks now recognise lava FluidStates
            plugins.put("com.legacy.nethercraft.items.capabilities.FluidNeridiumBucketWrapper", new PluginNethercraftClassic()); // fix nethercraft bucket capability
            plugins.put("com.legacy.nethercraft.items.tools.ItemNeridiumBucket", new PluginNethercraftClassic()); // make nethercraft's bucket use its IFluidHandler when placing/taking fluids
            plugins.put("com.lycanitesmobs.core.block.fluid.BlockFluidAcid", new PluginLycanitesFluidMixing.Acid()); // fix lycanites fluid mixing
            plugins.put("com.lycanitesmobs.core.block.fluid.BlockFluidMoglava", new PluginLycanitesFluidSources()); // fix lycanites fluid source logic
            plugins.put("com.lycanitesmobs.core.block.fluid.BlockFluidOoze", new PluginLycanitesFluidMixing.Ooze()); // fix lycanites fluid mixing
            plugins.put("com.lycanitesmobs.core.block.fluid.BlockFluidPoison", new PluginLycanitesFluidMixing.Poison()); // fix lycanites fluid mixing
            plugins.put("com.lycanitesmobs.core.block.fluid.BlockFluidRabbitooze", new PluginLycanitesFluidSources()); // fix lycanites fluid source logic
            plugins.put("com.lycanitesmobs.core.block.fluid.BlockFluidSharacid", new PluginLycanitesFluidSources()); // fix lycanites fluid source logic
            plugins.put("com.lycanitesmobs.core.block.fluid.BlockFluidVeshoney", new PluginLycanitesFluidMixing.Veshoney()); // fix lycanites fluid mixing
            plugins.put("com.lycanitesmobs.core.block.fluid.BlockFluidVesspoison", new PluginLycanitesFluidSources()); // fix lycanites fluid source logic
            plugins.put("com.lycanitesmobs.core.block.BlockFluidBase", new PluginLycanitesFluidBase()); // remove isEntityInsideMaterial override
            plugins.put("com.rwtema.extrautils2.utils.blockaccess.BlockAccessDelegate", new PluginExtraUtilsAccessDelegate()); // extrautils' block access wrapper FluidState-sensitive
            plugins.put("com.rwtema.extrautils2.utils.blockaccess.ThreadSafeBlockAccess", new PluginExtraUtilsAccessServer()); // extrautils' block access wrapper FluidState-sensitive
            plugins.put("com.teammetallurgy.atum.blocks.base.BlockAtumWall", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("crafttweaker.mc1120.block.MCWorldBlock", new PluginCraftTweaker()); // MCWorldBlock.getFluid can read FluidStates
            plugins.put("crazypants.enderio.base.fluid.BlockFluidEio$FireWater", new PluginEnderIO()); // fix fire water fluid collision
            plugins.put("crazypants.enderio.base.fluid.BlockFluidEio$VaporOfLevity", new PluginEnderIO()); // levity snow forming accounts for FluidStates
            plugins.put("de.ellpeck.actuallyadditions.mod.blocks.BlockWallAA", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("dev.necauqua.mods.cm.mixin.entity.EntityLivingBaseMixin", new PluginChiseledMe()); // fix chiseled me conflict
            plugins.put("dev.necauqua.mods.cm.mixin.entity.EntityMixin", new PluginChiseledMe()); // fix chiseled me conflict
            plugins.put("dev.necauqua.mods.cm.mixin.WorldMixin", new PluginChiseledMe()); // fix chiseled me conflict
            plugins.put("endreborn.mod.blocks.BlockWallBase", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("erebus.blocks.BlockWallErebus", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("exnihilocreatio.barrel.modes.fluid.BarrelModeFluid", new PluginExNihiloCreatio()); // allow "fluid on top" barrel crafting to accept FluidStates
            plugins.put("hellfirepvp.astralsorcery.common.block.fluid.FluidBlockLiquidStarlight", new PluginAstralSorcery()); // fixes weird mixing interactions
            plugins.put("io.github.lxgaming.sledgehammer.mixin.core.block.BlockDynamicLiquidMixin", new PluginSledgehammer()); // remove redundant transformer
            plugins.put("io.github.opencubicchunks.cubicchunks.core.asm.mixin.core.client.MixinChunkCache_HeightLimits", new PluginCubicChunks()); // fix mixin annotation to target fluidlogged api transform
            plugins.put("net.journey.items.JItemWaterLily", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("lumien.randomthings.item.ItemEnderBucket", new PluginRandomThings()); // make random things' ender buckets sensitive to FluidStates
            plugins.put("lumien.randomthings.item.ItemReinforcedEnderBucket", new PluginRandomThings()); // make random things' ender buckets sensitive to FluidStates
            plugins.put("mcp.mobius.waila.addons.core.PluginCore", new PluginWaila()); // remove duplicate handlers for BlockLiquid
            plugins.put("me.jellysquid.mods.phosphor.mod.world.lighting.LightingEngine", new PluginHesperus()); // phosphor takes FluidStates into account when computing light
            plugins.put("me.jellysquid.mods.phosphor.mod.world.lighting.LightingHooks", new PluginHesperus()); // phosphor takes FluidStates into account when computing light
            plugins.put("meldexun.nothirium.mc.renderer.chunk.SectionRenderCache", new PluginNothirium()); // nothirium compat
            plugins.put("micdoodle8.mods.galacticraft.core.blocks.BlockFluidGC", new PluginGalacticraft()); // fix rendering issues with certain galacticraft fluids
            plugins.put("micdoodle8.mods.galacticraft.core.blocks.BlockWallGC", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("mods.railcraft.common.fluids.CustomContainerHandler", new PluginRailcraft()); // fix railcraft uncraftable potion bug when collecting water bottles (issue#148)
            plugins.put("mrtjp.projectred.core.TFaceConnectable$class", new PluginProjectRed()); // allow wires to connect through fluids
            plugins.put("mrtjp.projectred.exploration.BlockDecorativeWall", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("net.dries007.tfc.objects.blocks.BlockFluidTFC", new PluginTFCBlockFluid()); // duplicate fluid logic isn't needed, and causes conflicts with this mod
            plugins.put("net.dries007.tfc.objects.fluids.FluidsTFC", new PluginTFCFluids()); // use ICompatibleFluid for water-like fluids
            plugins.put("net.optifine.override.ChunkCacheOF", new PluginOptifine()); // better optifine compat
            plugins.put("net.tropicraft.core.client.TropicraftWaterRenderFixer", new PluginTropicraftOverlays()); // account for FluidStates and improved fluid collisions
            plugins.put("net.tropicraft.core.common.block.BlockTropicraftFence", new PluginTropicraftFence()); // fixes for tropicraft fences
            plugins.put("net.tropicraft.core.common.block.BlockTropicraftSands", new PluginTropicraftSand()); // account for FluidStates
            plugins.put("net.tropicraft.core.common.fluid.FluidTropicsWater", new PluginTropicraftFluid()); // fix issue#183
            plugins.put("org.spongepowered.common.mixin.core.block.BlockDynamicLiquidMixin", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.block.BlockLiquidMixin", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.block.BlockStaticLiquidMixin", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.entity.EntityMixin", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.optimization.world.chunk.ChunkMixin_Async_Lighting", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.mod.mixin.core.forge.fluids.BlockFluidClassicMixin_Forge", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("paulevs.betternether.blocks.BNWall", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("plus.misterplus.plustweaks.mixins.MixinBlockFluidBase", new PluginPlusTweaks()); // fix crash with PlusTweaks mod fluid interactions
            plugins.put("plus.misterplus.plustweaks.mixins.MixinBlockLiquid", new PluginPlusTweaks()); // fix crash with PlusTweaks mod fluid interactions
            plugins.put("portablejim.bbw.core.WandWorker", new PluginBuildersWands()); // better builders wands compat
            plugins.put("stevekung.mods.moreplanets.planets.fronos.blocks.BlockFronosLilyPad", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("stevekung.mods.moreplanets.planets.fronos.item.ItemBlockFronosLilyPad", new PluginItemLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("stevekung.mods.moreplanets.utils.blocks.BlockFarmlandMP", new PluginBlockFarmland()); // farmland blocks now recognise water FluidStates
            plugins.put("stevekung.mods.moreplanets.utils.blocks.BlockWallMP", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("thebetweenlands.common.block.plant.BlockAlgae", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("thebetweenlands.common.block.structure.BlockWallBetweenlands", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("thebetweenlands.common.block.terrain.BlockLifeCrystalStalactite", new PluginBetweenlandsStates()); // keep FluidExtendedBlockState at the time of rendering
            plugins.put("thebetweenlands.common.block.terrain.BlockRootUnderwater", new PluginBetweenlandsStates()); // keep FluidExtendedBlockState at the time of rendering
            plugins.put("thebetweenlands.common.block.terrain.BlockRubber", new PluginBetweenlandsRubber()); // fix fluid collisions
            plugins.put("thebetweenlands.common.block.terrain.BlockStagnantWater", new PluginBetweenlandsRubber()); // fix fluid collisions
            plugins.put("thebetweenlands.common.block.terrain.BlockSwampWater", new PluginBetweenlands()); // betweenlands compat
            plugins.put("thebetweenlands.common.block.terrain.BlockTar", new PluginBetweenlandsRubber()); // fix fluid collisions
            plugins.put("thebetweenlands.common.entity.mobs.EntityTarBeast", new PluginBetweenlandsTarBeast()); // tar beast cannot be pushed by fluids
            plugins.put("thebetweenlands.common.item.ItemWaterPlaceable", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("thedarkcolour.futuremc.block.villagepillage.BlockWall", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("thelm.jaopca.additions.block.BlockWallBase", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("twilightforest.block.BlockTFHugeLilyPad", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("twilightforest.item.ItemBlockTFHugeLilyPad", new PluginTwilightForest()); // 2x2 lily pads can be placed on certain water FluidStates
            plugins.put("twilightforest.item.ItemBlockTFHugeWaterLily", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("vazkii.botania.common.item.ItemOpenBucket", new PluginBotania()); // allow botania's void bucket item to recognise tanks and FluidStates
            plugins.put("vazkii.botania.common.world.SkyblockWorldEvents", new PluginGardenOfGlass()); // wooden bowls can now be filled by using water FluidStates
            plugins.put("WayofTime.bloodmagic.block.BlockEnumWall", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("xreliquary.items.ItemEmperorChalice", new PluginReliquary()); // make reliquary's chalice use its IFluidHandler when placing/taking fluids
            // vanilla (blocks)
            plugins.put("net.minecraft.block.state.BlockStateBase", new PluginBlockStateBase()); // store one FluidState inside each BlockStateBase instance, this greatly increases the speed of fluid logic
            plugins.put("net.minecraft.block.state.BlockStateContainer", new PluginBlockStateContainer()); // store a level-to-FluidState lookup array in fluid block state containers
            plugins.put("net.minecraft.block.Block", new PluginBlock()); // fixes some lighting, canSustainPlant, and explosion related issues
            plugins.put("net.minecraft.block.BlockBarrier", new PluginBlockBarrier()); // move the hardcoded stuff from WorldClient to BlockBarrier
            plugins.put("net.minecraft.block.BlockBush", new PluginBlockBush()); // exists for fluidloggable plants that parent from this class
            plugins.put("net.minecraft.block.BlockCocoa", new PluginBlockCocoa()); // exists in case cocoa beans are added to the config whitelist
            plugins.put("net.minecraft.block.BlockConcretePowder", new PluginBlockConcretePowder()); // concrete forms from concrete powder while its next to flowing water FluidStates
            plugins.put("net.minecraft.block.BlockDoor", new PluginBlockDoor()); // update upper FluidState
            plugins.put("net.minecraft.block.BlockDynamicLiquid", new PluginBlockDynamicLiquid()); // fixes a bunch of liquid interactions while fluidlogged
            plugins.put("net.minecraft.block.BlockFarmland", new PluginBlockFarmland()); // farmland blocks now recognise water FluidStates
            plugins.put("net.minecraft.block.BlockFire", new PluginBlockFire()); // fire doesn't destroy fluidlogged fluids
            plugins.put("net.minecraft.block.BlockGrass", new PluginBlockGrass()); // use World#getBlockLightOpacity for FluidState sensitivity
            plugins.put("net.minecraft.block.BlockLilyPad", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("net.minecraft.block.BlockLiquid", new PluginBlockLiquid()); // significantly changes the BlockLiquid class to work with the mod
            plugins.put("net.minecraft.block.BlockMycelium", new PluginBlockMycelium()); // use World#getBlockLightOpacity for FluidState sensitivity
            plugins.put("net.minecraft.block.BlockReed", new PluginBlockReed()); // sugar cane blocks now recognise water FluidStates
            plugins.put("net.minecraft.block.BlockSkull", new PluginBlockSkull()); // wither skulls no longer void the FluidState here when summoning the wither
            plugins.put("net.minecraft.block.BlockSponge", new PluginBlockSponge()); // fixes drain interactions across all modded fluids & FluidStates
            plugins.put("net.minecraft.block.BlockStairs", new PluginBlockStairs()); // update neighboring fluids when this changes shape
            plugins.put("net.minecraft.block.BlockStaticLiquid", new PluginBlockStaticLiquid()); // update FluidStates
            plugins.put("net.minecraft.block.BlockTrapDoor", new PluginBlockTrapDoor()); // trapdoors now notify neighbors when opening/closing
            plugins.put("net.minecraft.block.BlockWall", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            // vanilla (client)
            plugins.put("net.minecraft.client.entity.EntityPlayerSP", new PluginEntityPlayerSP()); // disable sprint while in water
            plugins.put("net.minecraft.client.multiplayer.WorldClient", new PluginWorldClient()); // non-empty FluidStates call randomDisplayTick & move hardcoded barrier stuff to barrier.randomDisplayTick
            plugins.put("net.minecraft.client.particle.ParticleBubble", new PluginWaterParticles()); // this doesn't instantly disappear while inside water FluidStates
            plugins.put("net.minecraft.client.particle.ParticleDrip", new PluginWaterParticles()); // this doesn't instantly disappear while inside water FluidStates
            plugins.put("net.minecraft.client.particle.ParticleRain", new PluginParticleRain()); // fix all fluid-related rain collisions
            plugins.put("net.minecraft.client.particle.ParticleSuspend", new PluginWaterParticles()); // this doesn't instantly disappear while inside water FluidStates
            plugins.put("net.minecraft.client.renderer.chunk.RenderChunk", new PluginRenderChunk()); // allows the game to render FluidStates
            plugins.put("net.minecraft.client.renderer.ActiveRenderInfo", new PluginActiveRenderInfo()); // get block fog color from possible FluidState
            plugins.put("net.minecraft.client.renderer.EntityRenderer", new PluginEntityRenderer()); // fixes graphical underwater block selection; lava FluidStates now emit smoke while raining; fixes FluidState fog color
            // vanilla (entity)
            plugins.put("net.minecraft.entity.ai.EntityAIPanic", new PluginEntityAIPanic()); // water FluidStates are now seen as water blocks
            plugins.put("net.minecraft.entity.ai.RandomPositionGenerator", new PluginRandomPositionGenerator()); // water FluidStates are now seen as water blocks
            plugins.put("net.minecraft.entity.item.EntityBoat", new PluginEntityBoat()); // boats work with water FluidStates
            plugins.put("net.minecraft.entity.item.EntityItem", new PluginEntityItem()); // handle lava collisions correctly
            plugins.put("net.minecraft.entity.item.EntityXPOrb", new PluginEntityItem()); // handle lava collisions correctly
            plugins.put("net.minecraft.entity.projectile.EntityFishHook", new PluginEntityFishHook()); // fishhook entities generate the fishing particles at water FluidStates
            plugins.put("net.minecraft.entity.Entity", new PluginEntity());
            plugins.put("net.minecraft.entity.EntityLivingBase", new PluginEntityLivingBase()); // fix issue#151
            // vanilla (item)
            plugins.put("net.minecraft.item.ItemArmorStand", new PluginItemArmorStand()); // armor stands don't remove fluids at their position when placed
            plugins.put("net.minecraft.item.ItemBucket", new PluginItemBucket()); // make vanilla buckets use their IFluidHandler when placing/taking fluids
            plugins.put("net.minecraft.item.ItemGlassBottle", new PluginItemGlassBottle()); // glass bottles can now be filled by using water FluidStates
            plugins.put("net.minecraft.item.ItemLilyPad", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            // vanilla (world)
            plugins.put("net.minecraft.world.chunk.Chunk", new PluginChunk()); // account for FluidState light opacity & light values
            plugins.put("net.minecraft.world.chunk.ChunkPrimer", new PluginChunkPrimer()); // allow mods to generate FluidStates more optimally during world gen
            plugins.put("net.minecraft.world.end.DragonSpawnManager$3", new PluginDragonSpawnManager()); // summoning the ender dragon will now void FluidStates at the pillar locations
            plugins.put("net.minecraft.world.gen.feature.WorldGenDungeons", new PluginWorldGenDungeons()); // spawner dungeons now void FluidStates when they generate
            plugins.put("net.minecraft.world.gen.structure.template.Template", new PluginTemplate()); // structures can load saved FluidStates
            plugins.put("net.minecraft.world.ChunkCache", new PluginChunkCache()); // implements IChunkProvider
            plugins.put("net.minecraft.world.World", new PluginWorld()); // corrects a lot of FluidState related interactions
            plugins.put("net.minecraft.world.WorldServer", new PluginWorldServer()); // FluidStates now get ticked
        }

        @Nonnull
        @Override
        public String getPluginName() { return "Fluidlogged API Plugin"; }
    }

    @Nonnull
    @Override
    public String[] getASMTransformerClass() {
        return new String[] { getPluginClass(),
            "git.jbredwards.fluidlogged_api.mod.asm.transformers.TransformerLevelProperty",
            "git.jbredwards.fluidlogged_api.mod.asm.transformers.TransformerMethodRedirects",
            "git.jbredwards.fluidlogged_api.mod.asm.transformers.TransformerModdedBoats",
            "git.jbredwards.fluidlogged_api.mod.asm.transformers.TransformerSmoothWater"
        };
    }
}
