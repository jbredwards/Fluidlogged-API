package git.jbredwards.fluidlogged_api.mod.common.config;

import com.google.common.primitives.Ints;
import com.google.gson.*;
import git.jbredwards.fluidlogged_api.api.asm.impl.ICanFluidFlowHandler;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;
import net.minecraft.block.Block;
import net.minecraft.block.properties.IProperty;
import net.minecraft.block.state.IBlockState;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.Loader;
import net.minecraftforge.fml.common.eventhandler.Event;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.*;
import java.lang.reflect.Type;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.*;
import java.util.function.BiPredicate;

/**
 *
 * @author jbred
 *
 */
public final class FluidloggedAPIConfigHandler
{
    @Nonnull
    static final Gson GSON = new GsonBuilder()
            .registerTypeAdapter(ConfigPredicateBuilder.class, new ConfigPredicateDeserializer())
            .create();

    @Nonnull
    static final Logger LOGGER = LogManager.getFormatterLogger("Fluidlogged API Config");

    @Nonnull static final Map<String, Set<Fluid>> FLUID_TAGS_CACHE = new HashMap<>();
    @Nonnull public static final Map<Block, ConfigPredicate> WHITELIST = new HashMap<>();
    @Nonnull public static final Map<Block, ConfigPredicate> BLACKLIST = new HashMap<>();

    @Nullable
    static Config config;

    public static boolean applyDefaults = true;
    public static boolean fluidsBreakTorches = true;
    public static boolean debugASMPlugins = false;
    public static int fluidloggedFluidSpread = 2;
    public static boolean verticalFluidloggedFluidSpread = true;
    public static boolean lavalogVaporizeFlammable = false;

    //checks if the input state is fluidloggable, according to the config settings
    @Nonnull
    public static Event.Result isStateFluidloggable(@Nonnull IBlockState state, @Nullable Fluid fluid) {
        //check whitelist
        final @Nullable ConfigPredicate whitelist = WHITELIST.get(state.getBlock());
        if(whitelist != null && whitelist.test(state, fluid)) return Event.Result.ALLOW;
        //skip blacklist check
        else if(!applyDefaults) return Event.Result.DENY;
        //check blacklist
        final @Nullable ConfigPredicate blacklist = BLACKLIST.get(state.getBlock());
        if(blacklist != null && blacklist.test(state, fluid)) return Event.Result.DENY;
        //default
        return Event.Result.DEFAULT;
    }

    //initializes the config (internal use only!)
    public static void init() {
        final File cfg = new File("config", "fluidlogged_api.cfg");

        try {
            //creates a new cfg file
            if(cfg.createNewFile()) {
                //default config file contents
                final String contents =
                        "#determines how certain \"infinite\" fluids (ie. water not lava) can spread between fluidloggable blocks\n" +
                        "#case 0: fluids cannot flow into fluidloggable blocks\n" +
                        "#case 1: fluids can flow into fluidloggable blocks, but only from fluidlogged blocks (legacy mod behavior)\n" +
                        "#case 2: fluids can flow into fluidloggable blocks from fluidlogged blocks & normal fluid blocks (1.13 behavior)\n" +
                        "fluidloggedFluidSpread:2,\n" +
                        "\n" +
                        "#flowing fluid blocks break torches (vanilla behavior)\n" +
                        "fluidsBreakTorches:true,\n" +
                        "\n" +
                        "#this mod by default allows certain blocks to be fluidlogged\n" +
                        "applyDefaults:true,\n" +
                        "\n" +
                        "#true if flammable blocks should be destroyed when fluidlogged with a hot or burning liquid (like lava)\n" +
                        "lavalogVaporizeFlammable:false,\n" +
                        "\n" +
                        "#whitelist for adding new fluidloggable blocks (this is in addition to the defaults)\n" +
                        "#info about the format for this can be found on this mod's wiki:\n" +
                        "#https://github.com/jbredwards/Fluidlogged-API/wiki/Config\n" +
                        "whitelist:[],\n" +
                        "\n" +
                        "#blacklist blocks from the defaults\n" +
                        "#info about the format for this can be found on this mod's wiki:\n" +
                        "#https://github.com/jbredwards/Fluidlogged-API/wiki/Config\n" +
                        "blacklist:[],\n" +
                        "\n" +
                        "#otuput to the console for every ASM transformation, useful for debugging\n" +
                        "debugASMPlugins:false,\n" +
                        "\n" +
                        "#remove the ability for \"infinite\" fluids to fluidlog blocks below\n" +
                        "removeVerticalFluidloggedFluidSpread:false";

                //writes the default entries to the new cfg file
                final FileWriter writer = new FileWriter(cfg);
                writer.write(contents);
                writer.close();

                //ensure defaults are applied
                fluidloggedFluidSpread = 2;
                fluidsBreakTorches = true;
                applyDefaults = true;
                debugASMPlugins = true;
                verticalFluidloggedFluidSpread = true;
                lavalogVaporizeFlammable = false;
            }

            //reads an already existing cfg file
            else {
                config = GSON.fromJson('{' + IOUtils.toString(Files.newInputStream(cfg.toPath()), Charset.defaultCharset()) + '}', Config.class);

                fluidloggedFluidSpread = config.fluidloggedFluidSpread;
                fluidsBreakTorches = config.fluidsBreakTorches;
                applyDefaults = config.applyDefaults;
                debugASMPlugins = config.debugASMPlugins;
                verticalFluidloggedFluidSpread = !config.removeVerticalFluidloggedFluidSpread;
                lavalogVaporizeFlammable = config.lavalogVaporizeFlammable;

                if(ConfigPredicateDeserializer.containsMissingEntries) {
                    ConfigPredicateDeserializer.containsMissingEntries = false;
                    LOGGER.info("If you're unclear on how to format the whitelist or blacklist of the config, check out this mod's config wiki: https://github.com/jbredwards/Fluidlogged-API/wiki/Config");
                }
            }

            WHITELIST.clear();
            BLACKLIST.clear();
        }

        //oops
        catch(IOException e) { e.printStackTrace(); }
    }

    //sets up the whitelist & blacklist (internal use only!)
    public static void complete() throws IOException {
        //allow other mods to add to the whitelist & blacklist
        for(String modId : Loader.instance().getIndexedModList().keySet()) {
            //fluid tags
            final @Nullable InputStream fluidTags = Loader.class.getResourceAsStream(String.format("/assets/%s/fluidlogged_api/fluidTags.json", modId));
            if(fluidTags != null) readFluidTags(GSON.fromJson(IOUtils.toString(fluidTags, Charset.defaultCharset()), FluidTag[].class));
            //whitelist
            final @Nullable InputStream whitelist = Loader.class.getResourceAsStream(String.format("/assets/%s/fluidlogged_api/whitelist.json", modId));
            if(whitelist != null) readPredicates(WHITELIST, GSON.fromJson(IOUtils.toString(whitelist, Charset.defaultCharset()), ConfigPredicateBuilder[].class));
            //blacklist
            final @Nullable InputStream blacklist = Loader.class.getResourceAsStream(String.format("/assets/%s/fluidlogged_api/blacklist.json", modId));
            if(blacklist != null) readPredicates(BLACKLIST, GSON.fromJson(IOUtils.toString(blacklist, Charset.defaultCharset()), ConfigPredicateBuilder[].class));
            //clear fluid tags before reading the next mod, as to prevent possible conflicts
            FLUID_TAGS_CACHE.clear();
        }
        //gives the user final say regarding the whitelist & blacklist
        //config will be null if the file doesn't exist (first launch)
        if(config != null) {
            readFluidTags(config.fluidTags);
            readPredicates(WHITELIST, config.whitelist);
            readPredicates(BLACKLIST, config.blacklist);
            FLUID_TAGS_CACHE.clear();
            config = null;
        }
    }

    private static void readFluidTags(@Nullable FluidTag[] tags) {
        //no tags defined by config
        if(tags == null) return;

        //build tags
        for(FluidTag tag : tags) {
            if(FLUID_TAGS_CACHE.containsKey(tag.id))
                throw new JsonParseException("Fluidlogged API Config: two fluidTags found with the same id: " + tag.id + ", please either merge them or remove the duplicate");

            final Set<Fluid> fluids = new HashSet<>();
            for(String fluidName : tag.fluids) {
                @Nullable Block fluidBlock = Block.getBlockFromName(fluidName);
                @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(fluidBlock);

                if(fluid != null && FluidloggedUtils.isFluidloggableFluid(fluidBlock)) fluids.add(fluid);
                else throw new JsonParseException("Fluidlogged API Config: Unable to parse fluid: " + fluidName + " from fluidTag: " + tag.id);
            }

            FLUID_TAGS_CACHE.put(tag.id, fluids);
        }
    }

    private static void readPredicates(@Nonnull Map<Block, ConfigPredicate> map, @Nonnull ConfigPredicateBuilder[] builders) {
        for(ConfigPredicateBuilder builder : builders) {
            if(builder != ConfigPredicateDeserializer.EMPTY) {
                @Nullable ConfigPredicate predicate = builder.build();
                if(predicate != null) {
                    map.put(predicate.block, predicate);
                    ICanFluidFlowHandler.Accessor.setOverride(predicate.block, builder.canFluidFlow == null ?
                            (builder.useDeprecatedSideCheck ? ICanFluidFlowHandler.DEPRECATED_CHECK : null) :
                            (builder.canFluidFlow ? ICanFluidFlowHandler.ALWAYS_FLOW : ICanFluidFlowHandler.NEVER_FLOW));
                }
            }
        }
    }

    //gson
    public static class Config
    {
        public final int fluidloggedFluidSpread;
        public final boolean removeVerticalFluidloggedFluidSpread;
        public final boolean fluidsBreakTorches;
        public final boolean applyDefaults;
        public final @Nullable FluidTag[] fluidTags;
        public final ConfigPredicateBuilder[] whitelist;
        public final ConfigPredicateBuilder[] blacklist;
        public final boolean debugASMPlugins;
        public boolean lavalogVaporizeFlammable;

        public Config(int fluidloggedFluidSpread, boolean removeVerticalFluidloggedFluidSpread, boolean fluidsBreakTorches, boolean applyDefaults, @Nullable FluidTag[] fluidTags, ConfigPredicateBuilder[] whitelist, ConfigPredicateBuilder[] blacklist, boolean debugASMPlugins, boolean lavalogVaporizeFlammable) {
            this.fluidloggedFluidSpread = fluidloggedFluidSpread;
            this.removeVerticalFluidloggedFluidSpread = removeVerticalFluidloggedFluidSpread;
            this.fluidsBreakTorches = fluidsBreakTorches;
            this.applyDefaults = applyDefaults;
            this.fluidTags = fluidTags;
            this.whitelist = whitelist;
            this.blacklist = blacklist;
            this.debugASMPlugins = debugASMPlugins;
            this.lavalogVaporizeFlammable = lavalogVaporizeFlammable;
        }
    }

    //allows fluids to be added to the whitelist & blacklist based on fluidTags
    public static class FluidTag
    {
        @Nonnull protected final String id;
        @Nonnull protected final String[] fluids;

        public FluidTag(@Nonnull String id, @Nonnull String[] fluids) {
            this.id = id;
            this.fluids = fluids;
        }
    }

    //checks if the state & fluid match the criteria set by the cfg
    public static class ConfigPredicate implements BiPredicate<IBlockState, Fluid>
    {
        @Nonnull protected final Block block;
        @Nonnull protected final boolean[] metadata;
        @Nonnull protected final Set<Fluid> validFluids;

        public ConfigPredicate(@Nonnull Block block, @Nonnull int[] metadata, @Nonnull Set<Fluid> validFluids) {
            this.block = block;
            this.validFluids = validFluids;
            this.metadata = new boolean[metadata.length == 0 ? 0 : (Ints.max(metadata) + 1)];
            Arrays.fill(this.metadata, false);

            for(int meta : metadata) this.metadata[meta] = true;
        }

        @Override
        public boolean test(@Nonnull IBlockState stateIn, @Nullable Fluid fluidIn) {
            //check fluidIn
            if(fluidIn != null && !validFluids.isEmpty() && !validFluids.contains(fluidIn))
                return false;

            //check stateIn
            if(metadata.length == 0) return true;
            final int meta = block.getMetaFromState(stateIn);
            return metadata.length > meta && metadata[meta];
        }
    }

    //exists to allow the config to be loaded early
    public static class ConfigPredicateBuilder
    {
        @Nonnull protected final String blockName;
        @Nonnull protected final IntSet metadata;
        @Nonnull protected final JsonObject[] states;
        @Nonnull protected final String[] fluids;
        @Nonnull protected final String[] fluidTags;
        @Nullable protected final Boolean canFluidFlow;
        protected final boolean useDeprecatedSideCheck;

        public ConfigPredicateBuilder(@Nonnull String blockName, @Nonnull IntSet metadata, @Nonnull JsonObject[] states, @Nonnull String[] fluids, @Nonnull String[] fluidTags, @Nullable Boolean canFluidFlow, boolean useDeprecatedSideCheck) {
            this.metadata = metadata;
            this.states = states;
            this.fluids = fluids;
            this.fluidTags = fluidTags;
            this.canFluidFlow = canFluidFlow;
            this.blockName = blockName;
            this.useDeprecatedSideCheck = useDeprecatedSideCheck;
        }

        @Nullable
        public ConfigPredicate build() {
            final @Nullable Block block = Block.getBlockFromName(blockName);
            if(block == null) {
                LOGGER.warn(String.format("Unable to parse block from blockId: %s, skipping...", blockName));
                return null;
            }

            //handle states
            gatherStateMetadata(block, metadata, states);

            //handle fluids
            final Set<Fluid> validFluids = new HashSet<>();
            for(String fluidName : fluids) {
                @Nullable Block fluidBlock = Block.getBlockFromName(fluidName);
                @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(fluidBlock);
                if(fluid == null) fluid = FluidRegistry.getFluid(fluidName);

                if(fluid != null && FluidloggedUtils.isFluidloggableFluid(fluidBlock)) validFluids.add(fluid);
                else LOGGER.warn(String.format("Unable to parse fluid from fluids: %s, skipping...", fluidName));
            }

            //handle fluid tags
            for(String id : fluidTags) {
                @Nullable Set<Fluid> tag = FLUID_TAGS_CACHE.get(id);

                if(tag != null) validFluids.addAll(tag);
                else LOGGER.warn(String.format("Unable to parse tag from fluidTags: %s, skipping...", id));
            }

            return new ConfigPredicate(block, metadata.toIntArray(), validFluids);
        }

        //appends the metadata values of the serializedStates args to the provided metadata list
        public static void gatherStateMetadata(@Nonnull Block block, @Nonnull IntSet metadata, @Nonnull JsonObject... serializedStates) {
            for(JsonObject stateJson : serializedStates) {
                //sets up the properties that the state must have
                final List<Map.Entry<IProperty<?>, String[]>> validProperties = new ArrayList<>();
                for(Map.Entry<String, JsonElement> entry : stateJson.entrySet()) {
                    final IProperty<?> property = block.getBlockState().getProperty(entry.getKey());
                    if(property != null) {
                        final JsonElement validValuesJson = entry.getValue();
                        final Set<String> validValues = new HashSet<>();
                        if(!validValuesJson.isJsonArray()) validValues.add(validValuesJson.getAsString());
                        else validValuesJson.getAsJsonArray().forEach(element -> validValues.add(element.getAsString()));
                        validProperties.add(Pair.of(property, validValues.toArray(new String[0])));
                    }

                    //unknown property property
                    else System.out.printf("Unable to parse block property for %s: \"%s\", skipping property...", block.getRegistryName(), entry.getKey());
                }

                //serialize all valid states
                if(!validProperties.isEmpty()) {
                    for(IBlockState state : block.getBlockState().getValidStates()) {
                        gatherStateMetadataRecursively(validProperties, 0, validProperties.get(0).getKey(), state, metadata);
                    }
                }
            }
        }

        //internal, called by gatherStateMetadata
        static <T extends Comparable<T>> void gatherStateMetadataRecursively(@Nonnull List<Map.Entry<IProperty<?>, String[]>> validProperties, int propertyIndex, @Nonnull IProperty<T> property, @Nonnull IBlockState stateIn, @Nonnull IntSet metadata) {
            for(String valueStr : validProperties.get(propertyIndex).getValue()) {
                property.parseValue(valueStr).toJavaUtil().ifPresent(value -> {
                    final IBlockState state = stateIn.withProperty(property, value);
                    if(propertyIndex + 1 == validProperties.size()) metadata.add(state.getBlock().getMetaFromState(state));
                    else gatherStateMetadataRecursively(validProperties, propertyIndex + 1, validProperties.get(propertyIndex + 1).getKey(), state, metadata);
                });
            }
        }
    }

    //gson
    public static class ConfigPredicateDeserializer implements JsonDeserializer<ConfigPredicateBuilder>
    {
        @Nonnull
        protected static final ConfigPredicateBuilder EMPTY = new ConfigPredicateBuilder("", new IntOpenHashSet(), new JsonObject[0], new String[0], new String[0], null, false);
        protected static boolean containsMissingEntries = false;

        @Nonnull
        @Override
        public ConfigPredicateBuilder deserialize(@Nonnull JsonElement jsonIn, @Nullable Type typeOfT, @Nullable JsonDeserializationContext context) throws JsonParseException {
            if(jsonIn.isJsonObject()) {
                final JsonObject json = jsonIn.getAsJsonObject();
                //no blockId specified
                if(!json.has("blockId")) {
                    LOGGER.warn(String.format("required \"blockId\" string argument not found within args: %s, skipping...", json));
                    containsMissingEntries = true;
                    return EMPTY;
                }

                //get valid metadata values
                final IntSet metadata = new IntOpenHashSet();
                if(json.has("metadata")) {
                    final JsonElement metadataJson = json.get("metadata");
                    //allow one value to be provided
                    if(!metadataJson.isJsonArray()) metadata.add(metadataJson.getAsInt());
                    //allow multiple values to be provided
                    else metadataJson.getAsJsonArray().forEach(element -> metadata.add(element.getAsInt()));
                }

                //get valid state values
                final Set<JsonObject> states = new HashSet<>();
                if(json.has("states")) {
                    final JsonElement statesJson = json.get("states");
                    if(!statesJson.isJsonArray()) states.add(statesJson.getAsJsonObject());
                    else statesJson.getAsJsonArray().forEach(element -> states.add(element.getAsJsonObject()));
                }

                //get fluids
                final Set<String> fluids = new HashSet<>();
                if(json.has("fluids")) {
                    final JsonElement fluidsJson = json.get("fluids");
                    //allow one value to be provided
                    if(!fluidsJson.isJsonArray()) fluids.add(fluidsJson.getAsString());
                    //allow multiple values to be provided
                    else fluidsJson.getAsJsonArray().forEach(element -> fluids.add(element.getAsString()));
                }

                //get fluid tags
                final Set<String> fluidTags = new HashSet<>();
                if(json.has("fluidTags")) {
                    final JsonElement fluidTagsJson = json.get("fluidTags");
                    //allow one value to be provided
                    if(!fluidTagsJson.isJsonArray()) fluidTags.add(fluidTagsJson.getAsString());
                    //allow multiple values to be provided
                    else fluidTagsJson.getAsJsonArray().forEach(element -> fluidTags.add(element.getAsString()));
                }

                //get canFluidFlow
                final Boolean canFluidFlow = json.has("canFluidFlow") ? json.get("canFluidFlow").getAsBoolean() : null;

                //get useDeprecatedSideCheck
                final boolean useDeprecatedSideCheck = json.has("useDeprecatedSideCheck")
                        && json.get("useDeprecatedSideCheck").getAsBoolean();

                return new ConfigPredicateBuilder(
                        json.get("blockId").getAsString(), metadata,
                        states.toArray(new JsonObject[0]),
                        fluids.toArray(new String[0]),
                        fluidTags.toArray(new String[0]),
                        canFluidFlow, useDeprecatedSideCheck);
            }

            LOGGER.warn(String.format("bad json %s, skipping...", jsonIn));
            containsMissingEntries = true;
            return EMPTY;
        }
    }
}
