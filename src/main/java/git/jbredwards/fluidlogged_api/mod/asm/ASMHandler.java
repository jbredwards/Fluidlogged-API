/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.asm;

import git.jbredwards.fluidlogged_api.api.asm.AbstractClassTransformer;
import git.jbredwards.fluidlogged_api.api.asm.BasicLoadingPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.PluginFluidOrReal;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.*;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.actually_additions.PluginActuallyAdditions;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.appliedenergistics2.PluginAE2;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.astral_sorcery.PluginAstralSorcery;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.astral_sorcery.PluginAstralSorceryAccess;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.astral_sorcery.PluginEntityCrystal;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.bedrockores.PluginBedrockOre;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands.PluginBetweenlands;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands.PluginBetweenlandsRubber;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands.PluginBetweenlandsStates;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands.PluginBetweenlandsTarBeast;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.biomesoplenty.PluginBiomesOPlenty;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.biomesoplenty.PluginBucketEventHandler;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.blue_skies.PluginBlueSkies;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.botania.PluginBotania;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.botania.PluginGardenOfGlass;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.buildcraft.PluginBlockUtil;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.buildcraft.PluginTileFloodGate;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.buildcraft.PluginTilePump;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.builders_wands.PluginBuildersWands;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cb_multipart.PluginCBMultipart;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.ceramics.PluginItemClayBucket;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.chiseled_me.PluginChiseledMe;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.chiselsandbits.PluginBlockChiseled;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.chiselsandbits.PluginItemBlockChiseled;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cofhcore.PluginCoFHCore;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.crafttweaker.PluginCraftTweaker;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cubic_chunks.PluginCubicChunks;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cubic_chunks.PluginICube;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dsurround.PluginBiomeUtil;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dsurround.PluginScanner;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dsurround.PluginStormSplashRenderer;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dsurround.PluginStreamJetEffect;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dynamictrees.PluginBlockRootyWater;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dynamictrees.PluginModelRootyWater;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.endercore.PluginEnderCore;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.enderio.PluginEnderIO;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.enderio.PluginEnderIOSponge;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.exnihilo.PluginExNihiloCreatio;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.extrautils.PluginExtraUtilsAccessDelegate;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.extrautils.PluginExtraUtilsAccessServer;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.flopper.PluginFlopper;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft.PluginBlockGrating;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft.PluginGCBlocks;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft.PluginGalacticraft;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.hesperus.PluginHesperus;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.immersiveengineering.PluginFluidConcrete;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.industrial_foregoing.PluginIndustrialForegoing;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.industrial_foregoing.PluginPinkSlimeFluid;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.industrial_foregoing.PluginWaterResourceTile;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.industrial_renewal.PluginIndustrialRenewal;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs.PluginLycanitesFluidBase;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs.PluginLycanitesFluidMixing;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs.PluginLycanitesFluidSources;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.mekanism.PluginMekanismPump;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.mekanism.PluginMekanismTank;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.mekanism.PluginMekanismUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.nethercraft.PluginNethercraftClassic;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.nothirium.PluginNothirium;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.nuclearcraft.PluginNuclearCraft;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.openblocks.PluginOpenBlocks;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.optifine.PluginIResolvable;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.optifine.PluginOptifine;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.optifine.PluginSVertexBuilder;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.orelib.PluginOreLib;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.orelib.PluginWorldUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.plus_tweaks.PluginPlusTweaks;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.pneumaticcraft.PluginItemEmptyPCB;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.projecte.PluginProjectEAmulet;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.projecte.PluginProjectEProjectile;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.projectred.PluginProjectRed;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.quantumflux.PluginQuantumFlux;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.railcraft.PluginRailcraft;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.random_things.PluginRandomThings;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.reliquary.PluginReliquary;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.ruins.PluginRuins;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.schematica.PluginBlockList;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.schematica.PluginSchematicWorld;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.sledgehammer.PluginSledgehammer;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.spongeforge.PluginSpongeForge;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tfc.PluginTFCBlockFluid;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tfc.PluginTFCFluids;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_dynamics.PluginThermalDynamics;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_expansion.PluginThermalExpansion;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_foundation.PluginThermalAerotheum;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_foundation.PluginThermalFoundation;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_foundation.PluginThermalGlowstone;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tiny_progressions.PluginTinyProgressions;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tropicraft.*;
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
            // -----
            // forge
            // -----
            plugins.put("net.minecraftforge.client.model.ModelFluid", new PluginModelFluid()); // use custom baked model for fluids
            plugins.put("net.minecraftforge.client.model.ModelFluid$BakedFluid", new PluginModelFluid()); // fixes all issues with fluidlogged z-fighting
            plugins.put("net.minecraftforge.common.util.BlockSnapshot", new PluginBlockSnapshot()); // save FluidStates in block snapshots
            plugins.put("net.minecraftforge.common.ForgeHooks", new PluginForgeHooks()); // fix ForgeHooks#isInsideOfMaterial by allowing it to access stored fluid blocks
            plugins.put("net.minecraftforge.fluids.capability.wrappers.FluidBucketWrapper", new PluginFluidBucketWrapper()); // fixes modded ItemBucket item instances
            plugins.put("net.minecraftforge.fluids.BlockFluidBase", new PluginBlockFluidBase()); // modded fluids work properly with the mod & prevent startup crash
            plugins.put("net.minecraftforge.fluids.BlockFluidClassic", new PluginBlockFluidClassic()); // modded fluids work properly with the mod
            plugins.put("net.minecraftforge.fluids.BlockFluidFinite", new PluginBlockFluidFinite()); // modded finite fluids work properly with the mod
            plugins.put("net.minecraftforge.fluids.FluidRegistry$1", new PluginFluidWater()); // add water's biome colors to its fluid class
            plugins.put("net.minecraftforge.fluids.FluidUtil", new PluginFluidUtil()); // changes some of this class's util functions to be FluidState sensitive
            plugins.put("net.minecraftforge.fluids.UniversalBucket", new PluginUniversalBucket()); // only offset the fluid placement pos if the block isn't replaceable or fluidloggable
            // ------
            // modded
            // ------
            plugins.put("appeng.entity.EntityChargedQuartz", new PluginAE2()); // make ae2's fluix crystal creation FluidState-sensitive
            plugins.put("appeng.entity.EntityGrowingCrystal", new PluginAE2()); // make ae2's crystal seed growth FluidState-sensitive
            plugins.put("atomicstryker.ruins.common.RuinTemplate", new PluginRuins()); // prevent Ruins structures from being able to contain old FluidStates
            plugins.put("atomicstryker.ruins.common.RuinTemplateRule", new PluginRuins()); // prevent Ruins structures from being able to contain old FluidStates
            plugins.put("biomesoplenty.common.block.BlockBOPDirt", new PluginBlockGrass()); // fix grass & mycelium growing and not decaying underwater
            plugins.put("biomesoplenty.common.block.BlockBOPGrass", new PluginBlockGrass()); // fix grass & mycelium growing and not decaying underwater
            plugins.put("biomesoplenty.common.block.BlockBOPFarmland", new PluginBlockFarmland()); // farmland blocks now recognise water FluidStates
            plugins.put("biomesoplenty.common.fluids.blocks.BlockBloodFluid", new PluginBiomesOPlenty(false)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockHoneyFluid", new PluginBiomesOPlenty(false)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockHotSpringWaterFluid", new PluginBiomesOPlenty(false)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockPoisonFluid", new PluginBiomesOPlenty(true)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.fluids.blocks.BlockQuicksandFluid", new PluginBiomesOPlenty(true)); // fix BOP fluid block mixing
            plugins.put("biomesoplenty.common.handler.BucketEventHandler", new PluginBucketEventHandler()); // remove unnecessary event handler for BOP fluid bucket filling (and fixes a honey fluid dupe)
            plugins.put("biomesoplenty.common.item.ItemBOPLilypad", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("blusunrize.immersiveengineering.common.blocks.BlockIEFluidConcrete", new PluginFluidConcrete()); // fix issue#275
            plugins.put("buildcraft.factory.tile.TileFloodGate", new PluginTileFloodGate()); // make buildcraft's TileFloodGate FluidState-sensitive
            plugins.put("buildcraft.factory.tile.TilePump", new PluginTilePump()); // make buildcraft's pump account for FluidStates when checking for an infinite water source
            plugins.put("buildcraft.lib.misc.BlockUtil", new PluginBlockUtil()); // make buildcraft's BlockUtil fluid utility methods FluidState-sensitive
            plugins.put("cassiokf.industrialrenewal.blocks.abstracts.BlockMultiBlockBase", new PluginIndustrialRenewal()); // Fix industrial renewal mod's multi-blocks
            plugins.put("cassiokf.industrialrenewal.blocks.railroad.BlockCargoLoader", new PluginIndustrialRenewal()); // Fix industrial renewal mod's multi-blocks
            plugins.put("codechicken.multipart.BlockMultipart", new PluginCBMultipart()); // make multipart blocks fluidloggable by default
            plugins.put("codechicken.multipart.ItemPlacementHelper$", new PluginCBMultipart()); // allow multipart blocks to be placed in fluids
            plugins.put("codechicken.multipart.TileMultipart", new PluginCBMultipart()); // prevent desync when removing a fluidlogged multipart block
            plugins.put("cofh.core.fluid.BlockFluidCore", new PluginCoFHCore(false)); // check FluidloggedUtils::canFluidFlow before running fluid block interactions
            plugins.put("cofh.core.fluid.BlockFluidInteractive", new PluginCoFHCore(true)); // use this mod's fluid collision improvements
            plugins.put("cofh.core.proxy.EventHandlerRender", new PluginCoFHCore(false)); // fog and overlay rendering account for FluidStates
            plugins.put("cofh.thermaldynamics.duct.tiles.TileGrid", new PluginThermalDynamics()); // ray trace now skips fluids
            plugins.put("cofh.thermalexpansion.block.device.TileFisher", new PluginThermalExpansion()); // make thermal expansion's machines FluidState-sensitive
            plugins.put("cofh.thermalexpansion.block.device.TileWaterGen", new PluginThermalExpansion()); // make thermal expansion's machines FluidState-sensitive
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidAerotheum", new PluginThermalAerotheum()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidCryotheum", new PluginThermalFoundation()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidGlowstone", new PluginThermalGlowstone()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidMana", new PluginThermalFoundation()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidPetrotheum", new PluginThermalFoundation()); // fix conflicts
            plugins.put("cofh.thermalfoundation.fluid.BlockFluidPyrotheum", new PluginThermalFoundation()); // fix conflicts
            plugins.put("com.buuz135.industrial.proxy.BlockRegistry$1", new PluginPinkSlimeFluid()); // make industrial foregoing's pink slime fluid work better with FluidStates
            plugins.put("com.buuz135.industrial.tile.agriculture.WaterResourcesCollectorTile", new PluginWaterResourceTile(false)); // make industrial foregoing's water resource collector account for FluidStates
            plugins.put("com.buuz135.industrial.tile.misc.WaterCondesatorTile", new PluginWaterResourceTile(true)); // make industrial foregoing's water condesator account for FluidStates
            plugins.put("com.buuz135.industrial.tile.world.FluidPumpTile", new PluginIndustrialForegoing()); // make industrial foregoing's fluid pump FluidState-sensitive
            plugins.put("com.enderio.core.client.handlers.FluidVisualsHandler", new PluginEnderCore()); // make endercore's fluid overlay renderer FluidState-sensitive
            plugins.put("com.enderio.core.common.fluid.BlockFluidEnder", new PluginEnderCore()); // fix endercore fluid collisions
            plugins.put("com.enderio.core.common.util.IBlockAccessWrapper", new PluginEnderCore()); // make endercore's block access wrapper FluidState-sensitive
            plugins.put("com.ferreusveritas.dynamictrees.blocks.BlockRootyWater", new PluginBlockRootyWater()); // make DynamicTrees' water root block use actual fluidlogging, instead of pseudo fluidlogging
            plugins.put("com.ferreusveritas.dynamictrees.models.ModelRootyWater", new PluginModelRootyWater()); // remove water model, as the water is now handled via fluidlogging
            plugins.put("com.github.lunatrius.schematica.client.util.BlockList", new PluginBlockList()); // don't use chunks when getting IFluidHandler
            plugins.put("com.github.lunatrius.schematica.client.world.SchematicWorld", new PluginSchematicWorld()); // no FluidStates in schematics
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
            plugins.put("com.mcwdoors.kikoz.objects.blocks.Door", new PluginBlockDoor()); // update upper FluidState
            plugins.put("com.mcwdoors.kikoz.objects.blocks.JailDoor", new PluginBlockDoor()); // update upper FluidState
            plugins.put("com.mcwdoors.kikoz.objects.blocks.JapaneseDoors", new PluginBlockDoor()); // update upper FluidState
            plugins.put("com.mcwdoors.kikoz.objects.blocks.MetalDoor", new PluginBlockDoor()); // update upper FluidState
            plugins.put("com.mcwdoors.kikoz.objects.blocks.StableDoor", new PluginBlockDoor()); // update upper FluidState
            plugins.put("com.mcwdoors.kikoz.objects.blocks.Western", new PluginBlockDoor()); // update upper FluidState
            plugins.put("com.rwtema.extrautils2.utils.blockaccess.BlockAccessDelegate", new PluginExtraUtilsAccessDelegate()); // extrautils' block access wrapper FluidState-sensitive
            plugins.put("com.rwtema.extrautils2.utils.blockaccess.ThreadSafeBlockAccess", new PluginExtraUtilsAccessServer()); // extrautils' block access wrapper FluidState-sensitive
            plugins.put("crafttweaker.mc1120.block.MCWorldBlock", new PluginCraftTweaker()); // MCWorldBlock.getFluid can read FluidStates
            plugins.put("crazypants.enderio.base.block.insulation.BlockIndustrialInsulation", new PluginEnderIOSponge()); // make ender io's sponge (industrial insulation block) FluidState-sensitive
            plugins.put("crazypants.enderio.base.fluid.BlockFluidEio$FireWater", new PluginEnderIO()); // fix fire water fluid collision
            plugins.put("crazypants.enderio.base.fluid.BlockFluidEio$VaporOfLevity", new PluginEnderIO()); // levity snow forming accounts for FluidStates
            plugins.put("de.ellpeck.actuallyadditions.mod.blocks.BlockWallAA", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("de.ellpeck.actuallyadditions.mod.tile.TileEntityFluidCollector", new PluginActuallyAdditions()); // make actually additions' fluid placer/collector blocks FluidState-sensitive
            plugins.put("dev.necauqua.mods.cm.mixin.entity.EntityLivingBaseMixin", new PluginChiseledMe()); // fix chiseled me conflict
            plugins.put("dev.necauqua.mods.cm.mixin.entity.EntityMixin", new PluginChiseledMe()); // fix chiseled me conflict
            plugins.put("dev.necauqua.mods.cm.mixin.WorldMixin", new PluginChiseledMe()); // fix chiseled me conflict
            plugins.put("exnihilocreatio.barrel.modes.fluid.BarrelModeFluid", new PluginExNihiloCreatio()); // allow "fluid on top" barrel crafting to accept FluidStates
            plugins.put("hellfirepvp.astralsorcery.client.util.AirBlockRenderWorld", new PluginAstralSorceryAccess()); // make astral sorcery's block access wrappers FluidState-sensitive
            plugins.put("hellfirepvp.astralsorcery.client.util.BlockArrayRenderHelper$WorldBlockArrayRenderAccess", new PluginAstralSorceryAccess()); // make astral sorcery's block access wrappers FluidState-sensitive
            plugins.put("hellfirepvp.astralsorcery.client.util.RenderWorldBuffer", new PluginAstralSorceryAccess()); // make astral sorcery's block access wrappers FluidState-sensitive
            plugins.put("hellfirepvp.astralsorcery.common.block.fluid.FluidBlockLiquidStarlight", new PluginAstralSorcery()); // fixes weird mixing interactions
            plugins.put("hellfirepvp.astralsorcery.common.entities.EntityCrystal", new PluginEntityCrystal()); // make astral sorcery's starlight reactants FluidState-sensitive
            plugins.put("hellfirepvp.astralsorcery.common.entities.EntityCrystalTool", new PluginEntityCrystal()); // make astral sorcery's starlight reactants FluidState-sensitive
            plugins.put("hellfirepvp.astralsorcery.common.entities.EntityItemStardust", new PluginEntityCrystal()); // make astral sorcery's starlight reactants FluidState-sensitive
            plugins.put("hellfirepvp.astralsorcery.common.entities.EntityStarlightReacttant", new PluginEntityCrystal()); // make astral sorcery's starlight reactants FluidState-sensitive
            plugins.put("io.github.lxgaming.sledgehammer.mixin.core.block.BlockDynamicLiquidMixin", new PluginSledgehammer()); // remove redundant transformer
            plugins.put("io.github.opencubicchunks.cubicchunks.api.world.ICube", new PluginICube()); // make Cubic Chunks ICube implement ICubeData
            plugins.put("io.github.opencubicchunks.cubicchunks.core.asm.mixin.core.client.MixinChunkCache_HeightLimits", new PluginCubicChunks()); // fix mixin annotation to target fluidlogged api transform
            plugins.put("jotato.quantumflux.items.ItemVoidBucket", new PluginQuantumFlux()); // give void buckets an IFluidHandler, and account for FluidStates when draining fluids
            plugins.put("knightminer.ceramics.items.ItemClayBucket", new PluginItemClayBucket()); // offset the placement pos if the block is fluidloggable, so it can be fluidlogged
            plugins.put("li.cil.bedrockores.common.block.BlockBedrockOre", new PluginBedrockOre()); // fix issue#276
            plugins.put("lumien.randomthings.item.ItemEnderBucket", new PluginRandomThings()); // make random things' ender buckets sensitive to FluidStates
            plugins.put("lumien.randomthings.item.ItemReinforcedEnderBucket", new PluginRandomThings()); // make random things' ender buckets sensitive to FluidStates
            plugins.put("mcp.mobius.waila.addons.core.PluginCore", new PluginWaila()); // remove duplicate handlers for BlockLiquid
            plugins.put("me.desht.pneumaticcraft.common.item.ItemEmptyPCB", new PluginItemEmptyPCB()); // allow Empty PCBs to be filled using FluidStates
            plugins.put("me.jellysquid.mods.phosphor.mod.world.lighting.LightingEngine", new PluginHesperus()); // phosphor takes FluidStates into account when computing light
            plugins.put("me.jellysquid.mods.phosphor.mod.world.lighting.LightingHooks", new PluginHesperus()); // phosphor takes FluidStates into account when computing light
            plugins.put("mekanism.common.block.BlockMekanismContainer", new PluginBlockFlowerPot()); // fix FluidState voiding if a fluidlogged mekanism container is removed
            plugins.put("mekanism.common.block.BlockTileDrops", new PluginBlockFlowerPot()); // fix FluidState voiding if a fluidlogged mekanism block is removed
            plugins.put("mekanism.common.item.ItemBlockMachine", new PluginMekanismTank()); // make mekanism's fluid tank use its IFluidHandler when placing/taking
            plugins.put("mekanism.common.tile.TileEntityElectricPump", new PluginMekanismPump()); // make mekanism's electric pump FluidState-sensitive
            plugins.put("mekanism.common.util.MekanismUtils", new PluginMekanismUtils()); // make mekanism's fluid getter methods FluidState-sensitive
            plugins.put("meldexun.nothirium.mc.renderer.chunk.SectionRenderCache", new PluginNothirium()); // nothirium compat
            plugins.put("micdoodle8.mods.galacticraft.core.blocks.BlockFluidGC", new PluginGalacticraft(false)); // fix rendering issues with certain galacticraft fluids
            plugins.put("micdoodle8.mods.galacticraft.core.blocks.BlockGrating", new PluginBlockGrating()); // make Galacticraft's grating block use actual fluidlogging, instead of pseudo fluidlogging
            plugins.put("micdoodle8.mods.galacticraft.core.util.FluidUtil", new PluginGalacticraft(true)); // make galacticraft fluid fog texture overlay rendering account for FluidStates
            plugins.put("micdoodle8.mods.galacticraft.core.GCBlocks", new PluginGCBlocks()); // don't register Galacticraft's water & lava grating blocks, so they can be remapped
            plugins.put("micdoodle8.mods.galacticraft.planets.mars.client.fx.ParticleDrip", new PluginFluidOrReal("func_189213_a", "onUpdate")); // drip particles collide with FluidStates
            plugins.put("mod.chiselsandbits.chiseledblock.BlockChiseled", new PluginBlockChiseled()); // return null by default, to use built-in fluid collision logic for non-fluid chisel blocks
            plugins.put("mod.chiselsandbits.chiseledblock.ItemBlockChiseled", new PluginItemBlockChiseled(16)); // allow chisel blocks to be placed in replaceable blocks
            plugins.put("mod.chiselsandbits.network.packets.PacketChisel", new PluginItemBlockChiseled(18)); // allow chisel blocks to be placed in replaceable blocks
            plugins.put("mods.railcraft.common.fluids.CustomContainerHandler", new PluginRailcraft()); // fix railcraft uncraftable potion bug when collecting water bottles (issue#148)
            plugins.put("moze_intel.projecte.gameObjs.entity.EntityLavaProjectile", new PluginProjectEProjectile(false)); // allow the Volcanite Amulet projectile to lavalog blocks, and fix lava placement breaking blocks
            plugins.put("moze_intel.projecte.gameObjs.entity.EntityWaterProjectile", new PluginProjectEProjectile(true)); // allow the Evertide Amulet projectile to waterlog blocks
            plugins.put("moze_intel.projecte.gameObjs.items.EvertideAmulet", new PluginProjectEAmulet()); // allow the Evertide Amulet to waterlog blocks
            plugins.put("moze_intel.projecte.gameObjs.items.VolcaniteAmulet", new PluginProjectEAmulet()); // allow the Volcanite Amulet to lavalog blocks
            plugins.put("mrtjp.projectred.core.TFaceConnectable$class", new PluginProjectRed()); // allow wires to connect through fluids
            plugins.put("nc.block.fluid.BlockFluidGas", new PluginNuclearCraft()); // make source gas blocks only vaporize their FluidState while fluidlogged instead of the whole block
            plugins.put("nc.block.fluid.BlockFluidHotGas", new PluginNuclearCraft()); // make source hot gas blocks only vaporize their FluidState while fluidlogged instead of the whole block
            plugins.put("nc.block.fluid.BlockFluidParticle", new PluginNuclearCraft()); // make particle fluid blocks only vaporize their FluidState while fluidlogged instead of the whole block
            plugins.put("nc.block.fluid.BlockFluidSteam", new PluginNuclearCraft()); // make source steam blocks only vaporize their FluidState while fluidlogged instead of the whole block
            plugins.put("net.dries007.tfc.objects.blocks.BlockFluidTFC", new PluginTFCBlockFluid()); // duplicate fluid logic isn't needed, and causes conflicts with this mod
            plugins.put("net.dries007.tfc.objects.fluids.FluidsTFC", new PluginTFCFluids()); // use ICompatibleFluid for water-like fluids
            plugins.put("net.journey.items.JItemWaterLily", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("net.optifine.override.ChunkCacheOF", new PluginOptifine()); // better optifine compat
            plugins.put("net.optifine.reflect.ReflectorClass", new PluginIResolvable()); // fix crashes caused by Optifine loading classes too early
            plugins.put("net.optifine.reflect.ReflectorConstructor", new PluginIResolvable()); // fix crashes caused by Optifine loading classes too early
            plugins.put("net.optifine.reflect.ReflectorField", new PluginIResolvable()); // fix crashes caused by Optifine loading classes too early
            plugins.put("net.optifine.reflect.ReflectorMethod", new PluginIResolvable()); // fix crashes caused by Optifine loading classes too early
            plugins.put("net.optifine.shaders.SVertexBuilder", new PluginSVertexBuilder()); // set the block renderType (aka mc_Entity.y) to 1 if the block is a BlockLiquid
            plugins.put("net.tropicraft.core.client.TropicraftWaterRenderFixer", new PluginTropicraftOverlays()); // account for FluidStates and improved fluid collisions
            plugins.put("net.tropicraft.core.common.block.BlockTropicraftFence", new PluginTropicraftFence()); // fixes for tropicraft fences
            plugins.put("net.tropicraft.core.common.block.BlockTropicraftSands", new PluginTropicraftSand()); // account for FluidStates
            plugins.put("net.tropicraft.core.common.block.BlockTropicsWater", new PluginTropicraftFluid(true)); // fix flow direction
            plugins.put("net.tropicraft.core.common.entity.passive.EntityFishHook", new PluginEntityFishHook()); // fishhook entities generate the fishing particles at water FluidStates
            plugins.put("net.tropicraft.core.common.event.ItemEvents", new PluginTropicraftBucket()); // account for FluidStates
            plugins.put("net.tropicraft.core.common.fluid.FluidTropicsWater", new PluginTropicraftFluid(false)); // fix issue#183
            plugins.put("openblocks.common.block.BlockSponge", new PluginOpenBlocks()); // make openblocks' sponge FluidState-sensitive
            plugins.put("org.cyclops.flopper.tileentity.TileFlopper", new PluginFlopper()); // make floppers account for FluidStates
            plugins.put("org.orecruncher.dsurround.client.fx.particle.ParticleDripOverride", new PluginFluidOrReal(false, true, "firstTime", "firstTime", "func_189213_a", "onUpdate")); // drip particles collide with FluidStates
            plugins.put("org.orecruncher.dsurround.client.fx.JetEffect", new PluginStreamJetEffect()); // account for FluidStates when checking surrounding blocks
            plugins.put("org.orecruncher.dsurround.client.fx.SteamJetEffect", new PluginStreamJetEffect()); // account for FluidStates and side solidity
            plugins.put("org.orecruncher.dsurround.client.fx.WaterSplashJetEffect", new PluginFluidOrReal(false, false, "isUnboundedLiquid", "liquidBlockCount", "isValidSpawnBlock")); // account for FluidStates
            plugins.put("org.orecruncher.dsurround.client.handlers.effects.BreathEffect", new PluginFluidOrReal("getHeadBlock")); // make the underwater breathing effect account for FluidStates
            plugins.put("org.orecruncher.dsurround.client.handlers.scanners.AlwaysOnBlockEffectScanner", new PluginScanner()); // also scan FluidStates
            plugins.put("org.orecruncher.dsurround.client.handlers.scanners.RandomBlockEffectScanner", new PluginScanner()); // also scan FluidStates
            plugins.put("org.orecruncher.dsurround.client.renderer.weather.StormSplashRenderer", new PluginStormSplashRenderer()); // account for FluidStates and fix particle y position
            plugins.put("org.orecruncher.dsurround.lib.scanner.CuboidScanner", new PluginScanner()); // also scan FluidStates
            plugins.put("org.orecruncher.dsurround.lib.scanner.Scanner", new PluginScanner()); // also scan FluidStates
            plugins.put("org.orecruncher.dsurround.registry.biome.BiomeUtil", new PluginBiomeUtil()); // account for FluidStates
            plugins.put("org.orecruncher.lib.chunk.DirectChunkCache", new PluginOreLib(true)); // allow OreLib's IBlockAccessEx to read FluidStates
            plugins.put("org.orecruncher.lib.chunk.PassThroughChunkCache", new PluginOreLib(false)); // allow OreLib's IBlockAccessEx to read FluidStates
            plugins.put("org.orecruncher.lib.WorldUtils", new PluginWorldUtils()); // make WorldUtils account for FluidStates
            plugins.put("org.spongepowered.common.mixin.core.block.BlockDynamicLiquidMixin", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.block.BlockLiquidMixin", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.block.BlockStaticLiquidMixin", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.core.entity.EntityMixin", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.common.mixin.optimization.world.chunk.ChunkMixin_Async_Lighting", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("org.spongepowered.mod.mixin.core.forge.fluids.BlockFluidClassicMixin_Forge", new PluginSpongeForge()); // spongeforge no longer mixins into conflicting methods
            plugins.put("plus.misterplus.plustweaks.mixins.MixinBlockFluidBase", new PluginPlusTweaks()); // fix crash with PlusTweaks mod fluid interactions
            plugins.put("plus.misterplus.plustweaks.mixins.MixinBlockLiquid", new PluginPlusTweaks()); // fix crash with PlusTweaks mod fluid interactions
            plugins.put("portablejim.bbw.core.WandWorker", new PluginBuildersWands()); // better builders wands compat
            plugins.put("stevekung.mods.moreplanets.planets.fronos.blocks.BlockFronosLilyPad", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("stevekung.mods.moreplanets.planets.fronos.item.ItemBlockFronosLilyPad", new PluginItemLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("stevekung.mods.moreplanets.utils.blocks.BlockFarmlandMP", new PluginBlockFarmland()); // farmland blocks now recognise water FluidStates
            plugins.put("thebetweenlands.common.block.plant.BlockAlgae", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("thebetweenlands.common.block.terrain.BlockLifeCrystalStalactite", new PluginBetweenlandsStates()); // keep FluidExtendedBlockState at the time of rendering
            plugins.put("thebetweenlands.common.block.terrain.BlockRootUnderwater", new PluginBetweenlandsStates()); // keep FluidExtendedBlockState at the time of rendering
            plugins.put("thebetweenlands.common.block.terrain.BlockRubber", new PluginBetweenlandsRubber()); // fix fluid collisions
            plugins.put("thebetweenlands.common.block.terrain.BlockStagnantWater", new PluginBetweenlandsRubber()); // fix fluid collisions
            plugins.put("thebetweenlands.common.block.terrain.BlockSwampWater", new PluginBetweenlands()); // betweenlands compat
            plugins.put("thebetweenlands.common.block.terrain.BlockTar", new PluginBetweenlandsRubber()); // fix fluid collisions
            plugins.put("thebetweenlands.common.entity.mobs.EntityTarBeast", new PluginBetweenlandsTarBeast()); // tar beast cannot be pushed by fluids
            plugins.put("thebetweenlands.common.item.ItemWaterPlaceable", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("twilightforest.block.BlockTFHugeLilyPad", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("twilightforest.item.ItemBlockTFHugeLilyPad", new PluginTwilightForest()); // 2x2 lily pads can be placed on certain water FluidStates
            plugins.put("twilightforest.item.ItemBlockTFHugeWaterLily", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("vazkii.botania.common.item.ItemOpenBucket", new PluginBotania()); // allow botania's void bucket item to recognise tanks and FluidStates
            plugins.put("vazkii.botania.common.world.SkyblockWorldEvents", new PluginGardenOfGlass()); // wooden bowls can now be filled by using water FluidStates
            plugins.put("xreliquary.items.ItemEmperorChalice", new PluginReliquary()); // make reliquary's chalice use its IFluidHandler when placing/taking fluids
            // -------
            // vanilla
            // -------
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
            plugins.put("net.minecraft.block.BlockFlowerPot", new PluginBlockFlowerPot()); // fix FluidState voiding if a fluidlogged flower pot is removed
            plugins.put("net.minecraft.block.BlockFire", new PluginBlockFire()); // fire doesn't destroy fluidlogged fluids
            plugins.put("net.minecraft.block.BlockGrass", new PluginBlockGrass()); // fix grass growing and not decaying underwater
            plugins.put("net.minecraft.block.BlockLilyPad", new PluginBlockLilyPad()); // lily pads can stay on certain water FluidStates
            plugins.put("net.minecraft.block.BlockLiquid", new PluginBlockLiquid()); // significantly changes the BlockLiquid class to work with the mod
            plugins.put("net.minecraft.block.BlockMycelium", new PluginBlockGrass()); // fix mycelium growing and not decaying underwater
            plugins.put("net.minecraft.block.BlockReed", new PluginBlockReed()); // sugar cane blocks now recognise water FluidStates
            plugins.put("net.minecraft.block.BlockSkull", new PluginBlockSkull()); // wither skulls no longer void the FluidState here when summoning the wither
            plugins.put("net.minecraft.block.BlockSponge", new PluginBlockSponge()); // fixes drain interactions across all modded fluids & FluidStates
            plugins.put("net.minecraft.block.BlockStairs", new PluginBlockStairs()); // update neighboring fluids when this changes shape
            plugins.put("net.minecraft.block.BlockStaticLiquid", new PluginBlockStaticLiquid()); // update FluidStates
            plugins.put("net.minecraft.block.BlockTrapDoor", new PluginBlockTrapDoor()); // trapdoors now notify neighbors when opening/closing
            plugins.put("net.minecraft.block.BlockWall", new PluginBlockWall()); // fixes a bug with walls that caused the post to unintentionally render
            plugins.put("net.minecraft.client.entity.EntityPlayerSP", new PluginEntityPlayerSP()); // disable sprint while in water
            plugins.put("net.minecraft.client.multiplayer.WorldClient", new PluginWorldClient()); // non-empty FluidStates call randomDisplayTick & move hardcoded barrier stuff to barrier.randomDisplayTick
            plugins.put("net.minecraft.client.particle.ParticleBubble", new PluginWaterParticles()); // this doesn't instantly disappear while inside water FluidStates
            plugins.put("net.minecraft.client.particle.ParticleDrip", new PluginFluidOrReal("func_189213_a", "onUpdate")); // drip particles collide with FluidStates
            plugins.put("net.minecraft.client.particle.ParticleRain", new PluginParticleRain()); // fix all fluid-related rain collisions
            plugins.put("net.minecraft.client.particle.ParticleSuspend", new PluginWaterParticles()); // this doesn't instantly disappear while inside water FluidStates
            plugins.put("net.minecraft.client.renderer.chunk.RenderChunk", new PluginRenderChunk()); // allows the game to render FluidStates
            plugins.put("net.minecraft.client.renderer.ActiveRenderInfo", new PluginActiveRenderInfo()); // get block fog color from possible FluidState
            plugins.put("net.minecraft.client.renderer.BlockFluidRenderer", new PluginBlockFluidRenderer()); // allow the vanilla fluid renderer to recognize FluidStates
            plugins.put("net.minecraft.client.renderer.EntityRenderer", new PluginEntityRenderer()); // fixes graphical underwater block selection; lava FluidStates now emit smoke while raining; fixes FluidState fog color
            plugins.put("net.minecraft.entity.ai.EntityAIPanic", new PluginEntityAIPanic()); // water FluidStates are now seen as water blocks
            plugins.put("net.minecraft.entity.ai.RandomPositionGenerator", new PluginRandomPositionGenerator()); // water FluidStates are now seen as water blocks
            plugins.put("net.minecraft.entity.item.EntityBoat", new PluginEntityBoat()); // boats work with water FluidStates
            plugins.put("net.minecraft.entity.item.EntityItem", new PluginEntityItem()); // handle lava collisions correctly
            plugins.put("net.minecraft.entity.item.EntityMinecart", new PluginEntityMinecart()); // minecarts account for fluids when applying drag
            plugins.put("net.minecraft.entity.item.EntityXPOrb", new PluginEntityItem()); // handle lava collisions correctly
            plugins.put("net.minecraft.entity.projectile.EntityFishHook", new PluginEntityFishHook()); // fishhook entities generate the fishing particles at water FluidStates
            plugins.put("net.minecraft.entity.Entity", new PluginEntity());
            plugins.put("net.minecraft.entity.EntityLivingBase", new PluginEntityLivingBase()); // fix issue#151
            plugins.put("net.minecraft.item.ItemArmorStand", new PluginItemArmorStand()); // armor stands don't remove fluids at their position when placed
            plugins.put("net.minecraft.item.ItemBucket", new PluginItemBucket()); // make vanilla buckets use their IFluidHandler when placing/taking fluids
            plugins.put("net.minecraft.item.ItemGlassBottle", new PluginItemGlassBottle()); // glass bottles can now be filled by using water FluidStates
            plugins.put("net.minecraft.item.ItemLilyPad", new PluginItemLilyPad()); // lily pads can be placed on certain water FluidStates
            plugins.put("net.minecraft.pathfinding.SwimNodeProcessor", new PluginSwimNodeProcessor()); // account for FluidStates
            plugins.put("net.minecraft.pathfinding.WalkNodeProcessor", new PluginWalkNodeProcessor()); // account for FluidStates
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
            "git.jbredwards.fluidlogged_api.mod.asm.transformers.TransformerModdedWalls",
            "git.jbredwards.fluidlogged_api.mod.asm.transformers.TransformerSmoothWater"
        };
    }
}
