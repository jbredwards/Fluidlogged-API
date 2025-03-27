/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.datafix.modded;

import git.jbredwards.fluidlogged_api.api.datafix.FluidMappingData;
import git.jbredwards.fluidlogged_api.api.datafix.IFluidloggedDataMapper;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import micdoodle8.mods.galacticraft.core.GCBlocks;
import micdoodle8.mods.galacticraft.core.GCFluids;
import micdoodle8.mods.galacticraft.core.blocks.BlockGrating;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;
import java.util.stream.Collectors;

/**
 *
 * @author jbred
 *
 */
public enum GalacticraftDataFixer implements IFluidloggedDataMapper
{
    INSTANCE;

    @Nullable private static Int2ObjectMap<FluidState> ID_FLUID_MAPPINGS;
    @Nullable private static Map<String, FluidState> NAME_FLUID_MAPPINGS;

    public static void register() {
        MinecraftForge.EVENT_BUS.register(INSTANCE);
        MAPPERS.put(Blocks.AIR, INSTANCE); // because Mapping::remap does not work properly for existing targets...
        MAPPERS.put(GCBlocks.grating, INSTANCE); // just in case Mapping::remap is fixed at some point...
        // create grating name to fluid lookup
        NAME_FLUID_MAPPINGS = BlockGrating.forgeBlocks.stream().collect(Collectors.toMap(
                g -> g.getTranslationKey().substring(5), // removes "tile." from keys
                g -> FluidState.of(g.getLiquidBlock(GCFluids.fluidOil.getBlock().getDefaultState()))));
        NAME_FLUID_MAPPINGS.put("grating1", FluidState.of(Blocks.WATER));
        NAME_FLUID_MAPPINGS.put("grating2", FluidState.of(Blocks.LAVA));
    }

    @SubscribeEvent
    public void remapBlocks(@Nonnull final RegistryEvent.MissingMappings<Block> event) {
        event.getAllMappings().stream()
                .filter(m -> m.key.getNamespace().equals("galacticraftcore"))
                .forEach(mapping -> {
                    @Nullable final FluidState fluid = NAME_FLUID_MAPPINGS.get(mapping.key.getPath());
                    if(fluid != null && fluid.isValid()) {
                        // store old id in id to fluid lookup
                        ID_FLUID_MAPPINGS.put(mapping.id, fluid);
                        // remap all fluid grating blocks to the empty grating block
                        // currently bugged, but I'm doing this in the hopes of it getting fixed in the future
                        mapping.remap(GCBlocks.grating);
                    }
                });
    }

    @Nullable
    @Override
    public FluidMappingData remapFluidData(final int blockID, final int blockMetadata) {
        if(blockID == 0) return null; // skip blocks that are truly air
        @Nullable final FluidState fluid = ID_FLUID_MAPPINGS.get(blockID);
        return fluid == null ? null : new FluidMappingData(
                FluidloggedAPIConfig.nonSourceFluidlogging ? fluid.withLevel(blockMetadata) : fluid.toSource()
        ).withBlock(GCBlocks.grating).withMetadata(0);
    }

    public static void init() { ID_FLUID_MAPPINGS = new Int2ObjectOpenHashMap<>(); }
    public static void reset() { ID_FLUID_MAPPINGS = null; }
}
