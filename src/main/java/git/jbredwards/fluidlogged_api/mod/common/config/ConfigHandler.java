package git.jbredwards.fluidlogged_api.mod.common.config;

import com.google.gson.*;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.util.AccessorUtils;
import it.unimi.dsi.fastutil.ints.Int2BooleanMap;
import it.unimi.dsi.fastutil.ints.Int2BooleanOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2BooleanMap;
import it.unimi.dsi.fastutil.objects.Object2BooleanOpenHashMap;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.JsonToNBT;
import net.minecraft.nbt.NBTException;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.EnumActionResult;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.Loader;
import org.apache.commons.io.IOUtils;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.*;
import java.lang.reflect.Type;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.BiPredicate;

/**
 *
 * @author jbred
 *
 */
public final class ConfigHandler
{
    @Nonnull
    private static final Gson GSON = new GsonBuilder()
            .registerTypeAdapter(ConfigPredicateBuilder.class, new ConfigPredicateDeserializer())
            .create();

    @Nonnull private static final Map<String, Set<Fluid>> FLUID_TAGS = new HashMap<>();
    @Nonnull public static final Map<Block, ConfigPredicate> WHITELIST = new HashMap<>();
    @Nonnull public static final Map<Block, ConfigPredicate> BLACKLIST = new HashMap<>();

    @Nullable
    private static Config config;

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
                        "\"fluidloggedFluidSpread\":2,\n" +
                        "\n" +
                        "#flowing fluid blocks break torches (vanilla behavior)\n" +
                        "\"fluidsBreakTorches\":true,\n" +
                        "\n" +
                        "#this mod by default allows certain blocks to be fluidlogged\n" +
                        "\"applyDefaults\":true,\n" +
                        "\n" +
                        "#whitelist for adding new fluidloggable blocks (this is in addition to the defaults)\n" +
                        "#info about the format for this can be found on this mod's wiki\n" +
                        "\"whitelist\":[],\n" +
                        "\n" +
                        "#blacklist blocks from the defaults\n" +
                        "#info about the format for this can be found on this mod's wiki\n" +
                        "\"blacklist\":[],\n" +
                        "\n" +
                        "#otuput to the console for every ASM transformation, useful for debugging\n" +
                        "\"debugASMPlugins\":false,\n" +
                        "\n" +
                        "#remove the ability for \"infinite\" fluids to fluidlog blocks below\n" +
                        "\"removeVerticalFluidloggedFluidSpread\":false";

                //writes the default entries to the new cfg file
                final FileWriter writer = new FileWriter(cfg);
                writer.write(contents);
                writer.close();
            }

            //reads an already existing cfg file
            else {
                config = GSON.fromJson('{' + IOUtils.toString(new FileInputStream(cfg), Charset.defaultCharset()) + '}', Config.class);

                fluidloggedFluidSpread = config.fluidloggedFluidSpread;
                fluidsBreakTorches = config.fluidsBreakTorches;
                applyDefaults = config.applyDefaults;
                debugASMPlugins = config.debugASMPlugins;
                verticalFluidloggedFluidSpread = !config.removeVerticalFluidloggedFluidSpread;
            }
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

    private static void readPredicates(Map<Block, ConfigPredicate> map, ConfigPredicateBuilder[] builders) {
        for(ConfigPredicateBuilder builder : builders) {
            ConfigPredicate predicate = builder.build();
            map.put(predicate.block, predicate);

            AccessorUtils.setCanFluidFlow(predicate.block, builder.canFluidFlow);
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
        @Nonnull protected final Int2BooleanMap metadata;
        @Nonnull protected final Object2BooleanMap<Fluid> validFluids;

        public ConfigPredicate(@Nonnull Block block, @Nonnull int[] metadata, @Nonnull Fluid[] validFluids) {
            this.block = block;
            this.metadata = new Int2BooleanOpenHashMap();
            this.validFluids = new Object2BooleanOpenHashMap<>();

            for(int meta : metadata) this.metadata.put(meta, true);
            for(Fluid fluid : validFluids) this.validFluids.put(fluid, true);
        }

        @Override
        public boolean test(@Nonnull IBlockState stateIn, @Nullable Fluid fluidIn) {
            //check fluidIn
            if(fluidIn != null && !validFluids.isEmpty() && !validFluids.containsKey(fluidIn))
                return false;

            //check stateIn
            return metadata.isEmpty() || metadata.containsKey(block.getMetaFromState(stateIn));
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

        public ConfigPredicateBuilder(@Nonnull String blockName, @Nonnull int[] metadata, @Nonnull String[] fluids, @Nonnull String[] fluidTags, @Nullable Boolean canFluidFlow) {
            this.metadata = metadata;
            this.fluids = fluids;
            this.fluidTags = fluidTags;
            this.canFluidFlow = canFluidFlow;
            this.blockName = blockName;
        }

        public ConfigPredicate build() {
            final @Nullable Block block = Block.getBlockFromName(blockName);
            if(block == null) throw new JsonParseException("Fluidlogged API Config: Unable to parse block from blockId: " + blockName);

            //handle fluids
            final Set<Fluid> validFluids = new HashSet<>();
            for(String fluidName : fluids) {
                @Nullable Block fluidBlock = Block.getBlockFromName(fluidName);
                @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(fluidBlock);

                if(fluid != null && FluidloggedUtils.isFluidloggableFluid(fluidBlock)) validFluids.add(fluid);
                else throw new JsonParseException("Fluidlogged API Config: Unable to parse fluid from fluids: " + fluidName);
            }

            //handle fluid tags
            for(String id : fluidTags) {
                @Nullable Set<Fluid> tag = FLUID_TAGS.get(id);

                if(tag != null) validFluids.addAll(tag);
                else throw new JsonParseException("Fluidlogged API Config: Unable to parse tag from fluidTags: " + id);
            }

            return new ConfigPredicate(block, metadata, validFluids.toArray(new Fluid[0]));
        }
    }

    //gson
    public static class ConfigPredicateDeserializer implements JsonDeserializer<ConfigPredicateBuilder>
    {
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

                    //get state meta
                    if(nbt.hasKey("metadata", Constants.NBT.TAG_INT_ARRAY))
                        metadata = nbt.getIntArray("metadata");

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

                    return new ConfigPredicateBuilder(blockName, metadata, fluids.toArray(new String[0]), fluidTags.toArray(new String[0]), canFluidFlow);
                }

                //no blockId specified
                else throw new JsonParseException("Fluidlogged API Config: blockId argument not found!");
            }

            //invalid json, probably
            catch(NBTException e) { throw new JsonParseException(e); }
        }
    }
}
