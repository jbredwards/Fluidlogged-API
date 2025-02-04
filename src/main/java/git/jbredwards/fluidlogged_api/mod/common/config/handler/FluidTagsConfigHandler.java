/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.config.handler;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.JsonUtils;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 *
 * @author jbred
 *
 */
public final class FluidTagsConfigHandler
{
    @Nonnull private static final String error = "An error occurred while parsing a fluid tag in file \"%s\", skipping...";
    @Nullable public static Multimap<String, Fluid> FLUID_TAGS = null;

    public static void init(@Nonnull final JsonObject configs) throws IOException {
        FLUID_TAGS = HashMultimap.create();

        // run for auto configs, mod instances, and user config
        FluidloggedAPIConfigs.forEach(configs, "FLUID_TAGS", "fluidTags", (file, jsonIn) -> {
            try {
                @Nonnull final JsonObject json = jsonIn.getAsJsonObject();
                @Nonnull final String id = JsonUtils.getString(json, "id");
                final boolean allowMissing = json.has("allowMissing") && JsonUtils.getBoolean(json.get("allowMissing"), "allowMissing");

                // remove the specified fluids from the tag
                if(json.has("remove")) {
                    @Nonnull final Set<Fluid> fluids = new HashSet<>();
                    FluidloggedAPIConfigs.getAsIterable(json.get("remove"), Function.identity()).forEach(element -> fluids.addAll(getFluids(id, element, allowMissing)));

                    if(fluids.isEmpty()) FLUID_TAGS.removeAll(id);
                    else FLUID_TAGS.putAll(id, Sets.difference(new HashSet<>(FLUID_TAGS.removeAll(id)), fluids));
                }

                // add the fluid tag
                else {
                    @Nonnull final Set<Fluid> fluids = new HashSet<>();
                    if(json.has("fluids")) FluidloggedAPIConfigs.getAsIterable(json.get("fluids"), Function.identity()).forEach(element -> fluids.addAll(getFluids(id, element, allowMissing)));
                    FLUID_TAGS.putAll(id, fluids);
                }
            }

            // if one fluidTag errors, don't skip the remaining ones as a result
            catch(@Nonnull final Throwable t) { new JsonParseException(String.format(error, file), t).printStackTrace(); }
        });
    }

    @Nonnull
    public static Collection<Fluid> getFluids(@Nonnull final String tagId, final boolean allowMissing) {
        if(FLUID_TAGS == null) throw new IllegalStateException("FluidTagsConfigHandler::getFluids can only be called while reading blacklist.cfg & whitelist.cfg!");

        @Nonnull final Collection<Fluid> fluids = FLUID_TAGS.get(tagId);
        if(allowMissing || !fluids.isEmpty()) return fluids;
        else throw new JsonParseException("Fluidlogged API Config: No fluids were found from fluidTag: \"" + tagId + '"');
    }

    @Nonnull
    public static Collection<Fluid> getFluids(@Nullable final String tagId, @Nonnull final JsonElement element, final boolean allowMissing) {
        if(element.isJsonPrimitive() || element.isJsonObject() && element.getAsJsonObject().has("fluidId")) {
            @Nullable final Fluid fluid = getFluid(tagId, element.isJsonObject() ? JsonUtils.getString(element.getAsJsonObject().get("fluidId"), "fluidId") : JsonUtils.getString(element, "fluidId"), allowMissing);
            return fluid != null ? Collections.singleton(fluid) : Collections.emptySet();
        }

        else if(element.isJsonObject()) {
            @Nonnull JsonObject json = element.getAsJsonObject();
            if(json.has("materialId")) {
                @Nullable final Fluid fluid = getFluid(tagId, JsonUtils.getString(json, "materialId"), allowMissing);
                if(fluid == null) return Collections.emptySet();
                else if(!fluid.canBePlacedInWorld()) throw new JsonParseException("Fluidlogged API Config: Cannot parse material: \"" + JsonUtils.getString(json, "materialId") + (tagId != null ? "\" in fluidTag: \"" + tagId + '"' : "\""));

                @Nonnull final Material material = fluid.getBlock().getDefaultState().getMaterial();
                @Nonnull final Collection<Fluid> fluids = FluidRegistry.getRegisteredFluids().values().stream()
                        .filter(fluidIn -> fluidIn.canBePlacedInWorld() && fluidIn.getBlock().getDefaultState().getMaterial() == material && !isNonFluidloggable(fluidIn.getBlock()))
                        .collect(Collectors.toSet());

                if(allowMissing || !fluids.isEmpty()) return fluids;
                else throw new JsonParseException("Fluidlogged API Config: No valid fluidloggable fluids were found from material: \"" + JsonUtils.getString(json, "materialId") + (tagId != null ? "\" in fluidTag: \"" + tagId + '"' : "\""));
            }
        }

        if(allowMissing) return Collections.emptySet();
        else throw new JsonParseException("Missing fluidId or materialId, expected to find a string");
    }

    @Nullable
    static Fluid getFluid(@Nullable final String tagId, @Nonnull final String fluidId, final boolean allowMissing) {
        @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(Block.getBlockFromName(fluidId));
        if(fluid == null) fluid = FluidRegistry.getFluid(fluidId);
        if(fluid == null) { // missing fluid, likely a bad config entry
            if(!allowMissing) throw new JsonParseException("Fluidlogged API Config: Unable to parse fluid: \"" + fluidId + (tagId != null ? "\" from fluidTag: \"" + tagId + '"' : "\""));
            else return null; // allow missing fluids if set, usually for optional mod compatibility
        }

        // only allow fluidloggable fluids to be fluidlogged
        else if(isNonFluidloggable(fluid.getBlock())) throw new JsonParseException("Fluidlogged API Config: Specified fluid cannot be fluidlogged: \"" + fluidId + (tagId != null ? "\" in fluidTag: \"" + tagId + '"' : "\""));
        else return fluid;
    }

    static boolean isNonFluidloggable(@Nullable final Block block) {
        if(!(block instanceof IFluidloggableFluid) || block.getRegistryName() == null) return true;

        // the block is a fluidloggable fluid for at least one of its states
        @Nonnull final IFluidloggableFluid handler = (IFluidloggableFluid)block;
        for(@Nonnull final IBlockState state : block.getBlockState().getValidStates()) if(handler.isFluidloggableFluid(FluidState.of(state))) return false;

        // none of the fluid's states are fluidloggable
        return true;
    }
}
