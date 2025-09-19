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

package git.jbredwards.fluidlogged_api.mod.common.config.util;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.primitives.Ints;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSyntaxException;
import git.jbredwards.fluidlogged_api.mod.asm.iface.ICanFluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigAccessor;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigFluidBox;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import git.jbredwards.fluidlogged_api.mod.common.config.handler.FluidTagsConfigHandler;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntIterator;
import it.unimi.dsi.fastutil.ints.IntList;
import net.minecraft.block.Block;
import net.minecraft.block.properties.IProperty;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.JsonUtils;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.fluids.Fluid;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 *
 * @author jbred
 *
 */
public class ConfigPredicateHelper
{
    @Nonnull final IntList metadata = new IntArrayList();
    @Nonnull final List<JsonObject> stateArgs = new ArrayList<>();
    @Nonnull final List<IConfigFluidBox.HeightBox> boxes;
    final boolean allowMissing;

    @Nonnull ConfigPredicate predicate;
    @Nullable final ICanFluidFlowHandler flowHandler;
    @Nonnull final ConfigPredicateOperation operation;

    public ConfigPredicateHelper(@Nonnull final JsonObject json, @Nonnull final UnaryOperator<ICanFluidFlowHandler> flowTransformer) {
        operation = json.has("replace") ? ConfigPredicateOperation.get(JsonUtils.getString(json.get("replace"), "replace")) : ConfigPredicateOperation.always;

        // ======================
        // BUILD CONFIG PREDICATE
        // ======================

        @Nonnull final Set<Fluid> fluids = new HashSet<>();
        @Nonnull final IntList fluidLevels = new IntArrayList();
        allowMissing = json.has("allowMissing") && JsonUtils.getBoolean(json.get("allowMissing"), "allowMissing");

        // gather metadata
        if(json.has("metadata")) FluidloggedAPIConfigs.getAsIterable(json.get("metadata"), element -> JsonUtils.getInt(element, "meta")).forEach(metadata::add);

        // gather state args
        if(json.has("states")) FluidloggedAPIConfigs.getAsIterable(json.get("states"), element -> JsonUtils.getJsonObject(element, "state")).forEach(stateArgs::add);

        // gather valid fluid levels
        if(json.has("fluidLevels")) FluidloggedAPIConfigs.getAsIterable(json.get("fluidLevels"), element -> JsonUtils.getInt(element, "fluidLevel")).forEach(fluidLevels::add);

        // gather valid fluids
        if(json.has("fluids")) FluidloggedAPIConfigs.getAsIterable(json.get("fluids"), Function.identity()).forEach(element -> fluids.addAll(FluidTagsConfigHandler.getFluids(null, element, allowMissing)));

        // gather valid fluids based on fluid tags
        if(json.has("fluidTags")) FluidloggedAPIConfigs.getAsIterable(json.get("fluidTags"), element -> JsonUtils.getString(element, "fluidTag")).forEach(tagId ->  fluids.addAll(FluidTagsConfigHandler.getFluids(tagId, allowMissing)));

        // gather fluid blocking boxes
        @Nonnull final List<IConfigFluidBox.HeightBox> builder = new ArrayList<>();
        if(json.has("boxes")) FluidloggedAPIConfigs.getAsIterable(json.get("boxes"), element -> JsonUtils.getJsonObject(element, "box")).forEach(box -> {
            @Nullable final JsonElement min = box.get("min"), max = box.get("max");

            if(min == null) throw new JsonSyntaxException("Missing min, expected to find a number");
            if(max == null) throw new JsonSyntaxException("Missing max, expected to find a number");

            if(!min.isJsonPrimitive() || !min.getAsJsonPrimitive().isNumber()) throw new JsonSyntaxException("Expected min to be a number, was " + JsonUtils.toString(min));
            if(!max.isJsonPrimitive() || !max.getAsJsonPrimitive().isNumber()) throw new JsonSyntaxException("Expected max to be a number, was " + JsonUtils.toString(max));

            builder.add(new IConfigFluidBox.HeightBox(min.getAsDouble() / 16, max.getAsDouble() / 16));
        }); boxes = Collections.unmodifiableList(builder);

        // create predicate that ignores the FluidState level
        if(fluidLevels.isEmpty()) {
            final boolean requireSource = json.has("requireSource") && JsonUtils.getBoolean(json.get("requireSource"), "requireSource");
            if(fluids.isEmpty()) predicate = requireSource ? ConfigPredicate.TRUE_FOR_SOURCE : ConfigPredicate.TRUE_FOR_SIDE;
            else predicate = requireSource
                    ? (world, pos, state, fluidState) -> ConfigPredicate.TRUE_FOR_SOURCE.test(world, pos, state, fluidState) && (fluidState.isEmpty() || fluids.contains(fluidState.getFluid()))
                    : (world, pos, state, fluidState) -> ConfigPredicate.TRUE_FOR_SIDE.test(world, pos, state, fluidState) && (fluidState.isEmpty() || fluids.contains(fluidState.getFluid()));
        }

        // create predicate that requires specific FluidState levels
        else {
            @Nonnull final boolean[] levels = new boolean[Ints.max(fluidLevels.toIntArray()) + 1];
            for(@Nonnull final IntIterator it = fluidLevels.iterator(); it.hasNext();) levels[it.nextInt()] = true;

            if(fluids.isEmpty()) predicate = (world, pos, state, fluidState) -> fluidState.isEmpty() || fluidState.getLevel() < levels.length && levels[fluidState.getLevel()];
            else predicate = (world, pos, state, fluidState) -> fluidState.isEmpty() || fluidState.getLevel() < levels.length && levels[fluidState.getLevel()] && fluids.contains(fluidState.getFluid());
        }

        // ========================
        // BUILD FLUID FLOW HANDLER
        // ========================

        if(json.has("canFluidFlow")) {
            @Nonnull final JsonElement canFluidFlow = json.get("canFluidFlow");
            if(canFluidFlow.isJsonPrimitive() && canFluidFlow.getAsJsonPrimitive().isBoolean()) flowHandler = flowTransformer.apply(canFluidFlow.getAsBoolean() ? ICanFluidFlowHandler.ALWAYS_FLOW : ICanFluidFlowHandler.NEVER_FLOW);
            else { // side-based canFluidFlow
                @Nonnull final boolean[] sides = new boolean[EnumFacing.VALUES.length];
                FluidloggedAPIConfigs.getAsIterable(canFluidFlow, JsonElement::getAsString).forEach(side -> sides[Objects.requireNonNull(EnumFacing.byName(side), () -> "Cannot get side from \"" + side + '"').getIndex()] = true);
                flowHandler = flowTransformer.apply((world, pos, state, side) -> sides[side.getIndex()]);
            }
        }

        else flowHandler = json.has("useDeprecatedSideCheck") && JsonUtils.getBoolean(json.get("useDeprecatedSideCheck"), "useDeprecatedSideCheck") ? ICanFluidFlowHandler.DEPRECATED_CHECK : null;
    }

    /**
     * Derives IBlockStates from this builder using the provided block as a base.
     * Afterwards, this applies config settings to each derived state.
     */
    public void forEachState(@Nonnull final Block base, @Nonnull final Function<IConfigAccessor, ConfigPredicate> getter, @Nonnull final BiConsumer<IConfigAccessor, ConfigPredicate> setter) {
        @Nonnull final List<IBlockState> states = new ArrayList<>();
        if(!stateArgs.isEmpty()) {
            stateArgs.forEach(json -> {
                // deserialize state properties and values
                @Nonnull final Multimap<IProperty<?>, String> properties = HashMultimap.create();
                json.entrySet().forEach(entry -> {
                    @Nullable final IProperty<?> prop = base.getBlockState().getProperty(entry.getKey());
                    if(prop != null) properties.putAll(prop, FluidloggedAPIConfigs.getAsIterable(entry.getValue(), JsonElement::getAsString));
                    else if(!allowMissing) throw new JsonParseException(String.format("Could not parse property \"%s\" from block \"%s\".", entry.getKey(), base.getRegistryName()));
                });

                // gather any states whose properties match the desired values
                if(!properties.isEmpty()) {
                    stateChecker:
                    for(@Nonnull final IBlockState state : base.getBlockState().getValidStates()) {
                        for(@Nonnull final IProperty<?> prop : properties.keySet()) if(!properties.containsEntry(prop, NBTUtil.getName(prop, state.getValue(prop)))) continue stateChecker;
                        states.add(state);
                    }
                }
            });

            // no states found with properties
            if(states.isEmpty() && !allowMissing) throw new JsonParseException(String.format("Could not parse any states from block \"%s\" with properties: %s", base.getRegistryName(), stateArgs));
        }

        // get states from metadata
        if(!metadata.isEmpty()) for(@Nonnull final IntIterator it = metadata.iterator(); it.hasNext();) states.add(base.getStateFromMeta(it.nextInt()));
        else if(stateArgs.isEmpty()) states.addAll(base.getBlockState().getValidStates()); // no specific states specified, assume all states are valid

        // run action for each parsed state
        for(@Nonnull final IConfigAccessor state : states.stream()
                .map(state -> (IConfigAccessor)(state instanceof IExtendedBlockState ? ((IExtendedBlockState)state).getClean() : state))
                .collect(Collectors.toSet())) {

            setter.accept(state, getter.apply(state) != null ? operation.apply(getter.apply(state), predicate) : predicate);
            if(flowHandler != null && (operation != ConfigPredicateOperation.never || ((ICanFluidFlowHandler.Accessor)state).getCanFluidFlowOverride() == null)) ICanFluidFlowHandler.Accessor.setOverride(state, flowHandler);
            ((IConfigFluidBox)state).setBoxes(boxes.isEmpty() ? null : boxes);
        }
    }
}
