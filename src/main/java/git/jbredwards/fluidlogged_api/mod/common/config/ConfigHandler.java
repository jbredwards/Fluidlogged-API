package git.jbredwards.fluidlogged_api.mod.common.config;

import com.google.common.primitives.Ints;
import com.google.gson.*;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.PluginBlock;
import it.unimi.dsi.fastutil.objects.Object2BooleanMap;
import it.unimi.dsi.fastutil.objects.Object2BooleanOpenHashMap;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.JsonToNBT;
import net.minecraft.nbt.NBTException;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.Loader;
import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.*;
import java.lang.reflect.Type;
import java.nio.charset.Charset;
import java.util.*;
import java.util.function.BiPredicate;

/**
 *
 * @author jbred
 *
 */
public final class ConfigHandler
{
    @Nonnull
    static final Gson GSON = new GsonBuilder()
            .registerTypeAdapter(ConfigPredicateBuilder.class, new ConfigPredicateDeserializer())
            .create();

    @Nonnull
    static final Logger LOGGER = LogManager.getFormatterLogger("Fluidlogged API Config");

    @Nonnull static final Map<String, Set<Fluid>> FLUID_TAGS = new HashMap<>();
    @Nonnull public static final Map<Block, ConfigPredicate> WHITELIST = new HashMap<>();
    @Nonnull public static final Map<Block, ConfigPredicate> BLACKLIST = new HashMap<>();

    @Nullable
    static Config config;

    public static boolean applyDefaults = true;
    public static boolean fluidsBreakTorches = true;
    public static boolean debugASMPlugins = false;
    public static int fluidloggedFluidSpread = 2;
    public static boolean verticalFluidloggedFluidSpread = true;

    //checks if the input state is fluidloggable, according to the config settings
    public static EnumActionResult isStateFluidloggable(@Nonnull IBlockState state, @Nullable Fluid fluid) {
        //check whitelist
        final @Nullable ConfigPredicate whitelist = WHITELIST.get(state.getBlock());
        if(whitelist != null && whitelist.test(state, fluid)) return EnumActionResult.SUCCESS;
        //skip blacklist check
        else if(!applyDefaults) return EnumActionResult.FAIL;
        //check blacklist
        final @Nullable ConfigPredicate blacklist = BLACKLIST.get(state.getBlock());
        if(blacklist != null && blacklist.test(state, fluid)) return EnumActionResult.FAIL;
        //default
        return EnumActionResult.PASS;
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
                        "#whitelist for adding new fluidloggable blocks (this is in addition to the defaults)\n" +
                        "#info about the format for this can be found on this mod's wiki:\n" +
                        "#https://github.com/CleanroomMC/Fluidlogged-API/wiki/Config\n" +
                        "whitelist:[],\n" +
                        "\n" +
                        "#blacklist blocks from the defaults\n" +
                        "#info about the format for this can be found on this mod's wiki:\n" +
                        "#https://github.com/CleanroomMC/Fluidlogged-API/wiki/Config\n" +
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
                debugASMPlugins = false;
                verticalFluidloggedFluidSpread = true;
            }

            //reads an already existing cfg file
            else {
                config = GSON.fromJson('{' + IOUtils.toString(new FileInputStream(cfg), Charset.defaultCharset()) + '}', Config.class);

                fluidloggedFluidSpread = config.fluidloggedFluidSpread;
                fluidsBreakTorches = config.fluidsBreakTorches;
                applyDefaults = config.applyDefaults;
                debugASMPlugins = config.debugASMPlugins;
                verticalFluidloggedFluidSpread = !config.removeVerticalFluidloggedFluidSpread;

                if(ConfigPredicateDeserializer.containsMissingEntries) {
                    ConfigPredicateDeserializer.containsMissingEntries = false;
                    LOGGER.info("If you're unclear on how to format the whitelist or blacklist of the config, check out this mod's config wiki: https://github.com/CleanroomMC/Fluidlogged-API/wiki/Config");
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
            FLUID_TAGS.clear();
        }
        //gives the user final say regarding the whitelist & blacklist
        //config will be null if the file doesn't exist (first launch)
        if(config != null) {
            readFluidTags(config.fluidTags);
            readPredicates(WHITELIST, config.whitelist);
            readPredicates(BLACKLIST, config.blacklist);
            FLUID_TAGS.clear();
            config = null;
        }
    }

    private static void readFluidTags(@Nullable FluidTag[] tags) {
        //no tags defined by config
        if(tags == null) return;

        //build tags
        for(FluidTag tag : tags) {
            if(FLUID_TAGS.containsKey(tag.id))
                throw new JsonParseException("Fluidlogged API Config: two fluidTags found with the same id: " + tag.id + ", please either merge them or remove the duplicate");

            final Set<Fluid> fluids = new HashSet<>();
            for(String fluidName : tag.fluids) {
                @Nullable Block fluidBlock = Block.getBlockFromName(fluidName);
                @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(fluidBlock);

                if(fluid != null && FluidloggedUtils.isFluidloggableFluid(fluidBlock)) fluids.add(fluid);
                else throw new JsonParseException("Fluidlogged API Config: Unable to parse fluid: " + fluidName + " from fluidTag: " + tag.id);
            }

            FLUID_TAGS.put(tag.id, fluids);
        }
    }

    private static void readPredicates(@Nonnull Map<Block, ConfigPredicate> map, @Nonnull ConfigPredicateBuilder[] builders) {
        for(ConfigPredicateBuilder builder : builders) {
            if(builder != ConfigPredicateDeserializer.EMPTY) {
                @Nullable ConfigPredicate predicate = builder.build();
                if(predicate != null) {
                    map.put(predicate.block, predicate);
                    ((PluginBlock.Accessor)predicate.block).setCanFluidFlow(builder.canFluidFlow == null ?
                            (builder.useDeprecatedSideCheck ? ICanFluidFlowHandler.DEPRECATED_CHECK : null) :
                            (builder.canFluidFlow ? ICanFluidFlowHandler.ALWAYS_FLOW : ICanFluidFlowHandler.NEVER_FLOW));
                }
            }
        }
    }

    //allows for custom canFluidFlow actions
    //TODO crafttweaker/groovyscript support maybe?
    @FunctionalInterface
    public interface ICanFluidFlowHandler
    {
        @Nonnull ICanFluidFlowHandler
                ALWAYS_FLOW = (world, pos, state, side) -> true,
                NEVER_FLOW = (world, pos, state, side) -> false,
                DEPRECATED_CHECK = (world, pos, state, side) -> !state.isSideSolid(world, pos, side);

        boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull EnumFacing side);
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

        public Config(int fluidloggedFluidSpread, boolean removeVerticalFluidloggedFluidSpread, boolean fluidsBreakTorches, boolean applyDefaults, @Nullable FluidTag[] fluidTags, ConfigPredicateBuilder[] whitelist, ConfigPredicateBuilder[] blacklist, boolean debugASMPlugins) {
            this.fluidloggedFluidSpread = fluidloggedFluidSpread;
            this.removeVerticalFluidloggedFluidSpread = removeVerticalFluidloggedFluidSpread;
            this.fluidsBreakTorches = fluidsBreakTorches;
            this.applyDefaults = applyDefaults;
            this.fluidTags = fluidTags;
            this.whitelist = whitelist;
            this.blacklist = blacklist;
            this.debugASMPlugins = debugASMPlugins;
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
        @Nonnull protected final Object2BooleanMap<Fluid> validFluids;

        public ConfigPredicate(@Nonnull Block block, @Nonnull int[] metadata, @Nonnull Fluid[] validFluids) {
            this.block = block;
            this.validFluids = new Object2BooleanOpenHashMap<>();
            this.metadata = new boolean[metadata.length == 0 ? 0 : Ints.max(metadata)];
            Arrays.fill(this.metadata, false);

            for(int meta : metadata) this.metadata[meta] = true;
            for(Fluid fluid : validFluids) this.validFluids.put(fluid, true);
        }

        @Override
        public boolean test(@Nonnull IBlockState stateIn, @Nullable Fluid fluidIn) {
            //check fluidIn
            if(fluidIn != null && !validFluids.isEmpty() && !validFluids.containsKey(fluidIn))
                return false;

            //check stateIn
            return metadata.length == 0 || metadata[block.getMetaFromState(stateIn)];
        }
    }

    //exists to allow the config to be loaded early
    public static class ConfigPredicateBuilder
    {
        @Nonnull protected final String blockName;
        @Nonnull protected final int[] metadata;
        @Nonnull protected final String[] fluids;
        @Nonnull protected final String[] fluidTags;
        @Nullable protected final Boolean canFluidFlow;
        protected final boolean useDeprecatedSideCheck;

        public ConfigPredicateBuilder(@Nonnull String blockName, @Nonnull int[] metadata, @Nonnull String[] fluids, @Nonnull String[] fluidTags, @Nullable Boolean canFluidFlow, boolean useDeprecatedSideCheck) {
            this.metadata = metadata;
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
                @Nullable Set<Fluid> tag = FLUID_TAGS.get(id);

                if(tag != null) validFluids.addAll(tag);
                else LOGGER.warn(String.format("Unable to parse tag from fluidTags: %s, skipping...", id));
            }

            return new ConfigPredicate(block, metadata, validFluids.toArray(new Fluid[0]));
        }
    }

    //gson
    public static class ConfigPredicateDeserializer implements JsonDeserializer<ConfigPredicateBuilder>
    {
        @Nonnull
        protected static final ConfigPredicateBuilder EMPTY = new ConfigPredicateBuilder("", new int[0], new String[0], new String[0], null, false);
        protected static boolean containsMissingEntries = false;

        @Nonnull
        @Override
        public ConfigPredicateBuilder deserialize(@Nonnull JsonElement json, @Nullable Type typeOfT, @Nullable JsonDeserializationContext context) throws JsonParseException {
            try {
                final NBTTagCompound nbt = JsonToNBT.getTagFromJson(json.toString());
                if(nbt.hasKey("blockId", Constants.NBT.TAG_STRING)) {
                    final String blockName = nbt.getString("blockId");
                    final Set<String> fluids = new HashSet<>();
                    final Set<String> fluidTags = new HashSet<>();
                    int[] metadata = new int[0];
                    Boolean canFluidFlow = null;
                    boolean useDeprecatedSideCheck = false;

                    //get state meta
                    if(nbt.hasKey("metadata", Constants.NBT.TAG_INT_ARRAY))
                        metadata = nbt.getIntArray("metadata");

                    //sometimes vanilla recognises the int array as a list for some reason
                    //this is the fix for now
                    else if(nbt.hasKey("metadata", Constants.NBT.TAG_LIST)) {
                        NBTTagList metaList = nbt.getTagList("metadata", Constants.NBT.TAG_INT);
                        metadata = new int[metaList.tagCount()];

                        for(int i = 0; i < metadata.length; i++)
                            metadata[i] = metaList.getIntAt(i);
                    }

                    //get fluids
                    if(nbt.hasKey("fluids", Constants.NBT.TAG_LIST)) {
                        final NBTTagList list = nbt.getTagList("fluids", Constants.NBT.TAG_STRING);
                        for(int i = 0; i < list.tagCount(); i++) fluids.add(list.getStringTagAt(i));
                    }
                    
                    //get fluid tags
                    if(nbt.hasKey("fluidTags", Constants.NBT.TAG_LIST)) {
                        final NBTTagList list = nbt.getTagList("fluidTags", Constants.NBT.TAG_STRING);
                        for(int i = 0; i < list.tagCount(); i++) fluidTags.add(list.getStringTagAt(i));
                    }

                    //get canFluidFlow
                    if(nbt.hasKey("canFluidFlow", Constants.NBT.TAG_BYTE))
                        canFluidFlow = nbt.getBoolean("canFluidFlow");

                    //get useDeprecatedSideCheck
                    if(nbt.hasKey("useDeprecatedSideCheck", Constants.NBT.TAG_BYTE))
                        useDeprecatedSideCheck = nbt.getBoolean("useDeprecatedSideCheck");

                    return new ConfigPredicateBuilder(blockName, metadata, fluids.toArray(new String[0]), fluidTags.toArray(new String[0]), canFluidFlow, useDeprecatedSideCheck);
                }

                //no blockId specified
                else {
                    LOGGER.warn(String.format("required \"blockId\" string argument not found within args: %s, skipping...", nbt));
                    containsMissingEntries = true;
                    return EMPTY;
                }
            }

            //invalid json, probably
            catch(NBTException e) { throw new JsonParseException(e); }
        }
    }
}
