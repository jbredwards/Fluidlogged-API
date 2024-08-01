/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.config.util;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.iface.ICanFluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigAccessor;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.JsonUtils;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fml.common.registry.ForgeRegistries;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.UnaryOperator;

/**
 *
 * @author jbred
 *
 */
@FunctionalInterface
public interface ConfigPredicate
{
    @Nonnull ConfigPredicate ALWAYS = (world, pos, state, fluidState) -> true;
    @Nonnull ConfigPredicate TRUE_FOR_SOURCE = (world, pos, state, fluidState) -> fluidState.isSource();
    @Nonnull ConfigPredicate TRUE_FOR_SIDE = (world, pos, state, fluidState) -> fluidState.isSource() || FluidloggedUtils.canFluidOccupy(state, world, pos, fluidState);

    /**
     * @return true if the input arguments match the predicate, otherwise false.
     */
    boolean test(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final FluidState fluidState);

    /**
     * Deserializes the ConfigPredicates using the provided json.
     */
    static void deserialize(@Nonnull final String fileName, @Nonnull final JsonObject json, @Nonnull final ConfigGetter configGetter, @Nonnull final ConfigSetter configSetter, @Nonnull final UnaryOperator<ICanFluidFlowHandler> flowTransformer) {
        @Nonnull final ConfigPredicateHelper helper = new ConfigPredicateHelper(json, flowTransformer);

        // deserialize blocks based on class
        if(json.has("classId")) FluidloggedAPIConfigs.getAsIterable(json.get("classId"), JsonElement::getAsString).forEach(classId -> {
            @Nonnull final Class<?> blockClass;

            try { blockClass = Class.forName(classId); }
            catch(@Nonnull final ClassNotFoundException e) {
                if(json.has("allowMissing") && JsonUtils.getBoolean(json.get("allowMissing"), "allowMissing")) return;
                new JsonParseException(String.format("Could not get class from \"%s\" in file \"%s\", skipping...", classId, fileName), e).printStackTrace();
                return;
            }

            // don't allow classes that every block in the game inherits from
            if(blockClass.isAssignableFrom(Block.class)) {
                new JsonParseException(String.format("Class \"%s\" is too broad in file \"%s\", skipping...", classId, fileName)).printStackTrace();
                return;
            }

            // generate config for each block that is an instanceof the class
            boolean foundBlock = false;
            for(@Nonnull final Map.Entry<ResourceLocation, Block> entry : ForgeRegistries.BLOCKS.getEntries()) {
                if(blockClass.isAssignableFrom(entry.getValue().getClass())) {
                    foundBlock = true;
                    try { helper.forEachState(entry.getValue(), configGetter, configSetter); }
                    // don't skip remaining blocks
                    catch(@Nonnull final Throwable t) {
                        @Nonnull final String error = "An error has occurred while deserializing config predicate for \"%s\" of class \"%s\" in file \"%s\", skipping...";
                        new JsonParseException(String.format(error, entry.getKey(), classId, fileName), t).printStackTrace();
                    }
                }
            }

            // no blocks were found with the provided class, alert the user
            if(!foundBlock && !(json.has("allowMissing") && JsonUtils.getBoolean(json.get("allowMissing"), "allowMissing")))
                new JsonParseException(String.format("Could not get any blocks from class \"%s\" in file \"%s\".", classId, fileName)).printStackTrace();
        });

        // deserialize blocks based on mod id
        else if(json.has("modId")) FluidloggedAPIConfigs.getAsIterable(json.get("modId"), JsonElement::getAsString).forEach(modId -> {
            // generate config for each block that is an instanceof the class
            boolean foundBlock = false;
            for(@Nonnull final Map.Entry<ResourceLocation, Block> entry : ForgeRegistries.BLOCKS.getEntries()) {
                if(modId.equals(entry.getKey().getNamespace())) {
                    foundBlock = true;
                    try { helper.forEachState(entry.getValue(), configGetter, configSetter); }
                    // don't skip remaining blocks
                    catch(@Nonnull final Throwable t) {
                        @Nonnull final String error = "An error has occurred while deserializing config predicate for \"%s\" from mod id \"%s\" in file \"%s\", skipping...";
                        new JsonParseException(String.format(error, entry.getKey(), modId, fileName), t).printStackTrace();
                    }
                }
            }

            // no blocks were found with the provided class, alert the user
            if(!foundBlock && !(json.has("allowMissing") && JsonUtils.getBoolean(json.get("allowMissing"), "allowMissing")))
                new JsonParseException(String.format("Could not get any blocks from mod id \"%s\" in file \"%s\".", modId, fileName)).printStackTrace();
        });

        // deserialize block based on id
        else if(!json.has("blockId")) throw new JsonParseException("Missing \"blockId\" for config predicate entry: " + json);
        else FluidloggedAPIConfigs.getAsIterable(json.get("blockId"), JsonElement::getAsString).forEach(blockId -> {
            @Nullable final Block block = Block.getBlockFromName(blockId);

            // don't create a predicate for invalid blocks
            if(block == null) {
                if(json.has("allowMissing") && JsonUtils.getBoolean(json.get("allowMissing"), "allowMissing")) return;
                new JsonParseException(String.format("Could not get block from \"%s\" in file \"%s\", skipping...", blockId, fileName)).printStackTrace();
                return;
            }

            // gather config predicates for states
            try { helper.forEachState(block, configGetter, configSetter); }
            catch(@Nonnull final Throwable t) {
                @Nonnull final String error = "An error has occurred while deserializing config predicate for \"%s\" in file \"%s\", skipping...";
                new JsonParseException(String.format(error, blockId, fileName), t).printStackTrace();
            }
        });
    }

    @FunctionalInterface
    interface ConfigGetter extends Function<IConfigAccessor, ConfigPredicate> {}

    @FunctionalInterface
    interface ConfigSetter extends BiConsumer<IConfigAccessor, ConfigPredicate> {}
}
