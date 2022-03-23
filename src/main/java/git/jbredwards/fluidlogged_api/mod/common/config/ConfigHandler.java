package git.jbredwards.fluidlogged_api.mod.common.config;

import com.google.gson.*;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
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
    private static final Gson GSON = new GsonBuilder().registerTypeAdapter(ConfigPredicateBuilder.class, new Deserializer()).create();

    @Nullable private static Config config;
    @Nonnull private static final Map<Block, ConfigPredicate> WHITELIST = new HashMap<>();
    @Nonnull private static final Map<Block, ConfigPredicate> BLACKLIST = new HashMap<>();

    private static boolean applyDefaults = true;
    public static boolean fluidsBreakTorches = true;
    public static boolean debugASMPlugins = false;
    public static int fluidloggedFluidSpread = 2;

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
                        "\"debugASMPlugins\":false";

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
            }
        }

        //oops
        catch(IOException e) { e.printStackTrace(); }
    }

    //sets up the whitelist & blacklist (internal use only!)
    public static void complete() throws IOException {
        //allow other mods to add to the whitelist & blacklist
        for(String modId : Loader.instance().getIndexedModList().keySet()) {
            //whitelist
            final @Nullable InputStream whitelist = Loader.class.getResourceAsStream(String.format("/assets/%s/fluidlogged_api/whitelist.json", modId));
            if(whitelist != null) readPredicates(WHITELIST, GSON.fromJson(IOUtils.toString(whitelist, Charset.defaultCharset()), ConfigPredicateBuilder[].class));

            //blacklist
            final @Nullable InputStream blacklist = Loader.class.getResourceAsStream(String.format("/assets/%s/fluidlogged_api/blacklist.json", modId));
            if(blacklist != null) readPredicates(BLACKLIST, GSON.fromJson(IOUtils.toString(blacklist, Charset.defaultCharset()), ConfigPredicateBuilder[].class));
        }

        //gives the user final say regarding the whitelist & blacklist
        if(config != null) {
            readPredicates(WHITELIST, config.whitelist);
            readPredicates(BLACKLIST, config.blacklist);
            config = null;
        }
    }

    private static void readPredicates(Map<Block, ConfigPredicate> map, ConfigPredicateBuilder[] builders) {
        for(ConfigPredicateBuilder builder : builders) {
            ConfigPredicate predicate = builder.build();
            map.put(predicate.block, predicate);
        }
    }

    //gson
    public static class Config
    {
        public final int fluidloggedFluidSpread;
        public final boolean fluidsBreakTorches;
        public final boolean applyDefaults;
        public final ConfigPredicateBuilder[] whitelist;
        public final ConfigPredicateBuilder[] blacklist;
        public final boolean debugASMPlugins;

        public Config(boolean enableLegacyCompat, int fluidloggedFluidSpread, boolean fluidsBreakTorches, boolean applyDefaults, ConfigPredicateBuilder[] whitelist, ConfigPredicateBuilder[] blacklist, boolean debugASMPlugins) {
            this.fluidloggedFluidSpread = fluidloggedFluidSpread;
            this.fluidsBreakTorches = fluidsBreakTorches;
            this.applyDefaults = applyDefaults;
            this.whitelist = whitelist;
            this.blacklist = blacklist;
            this.debugASMPlugins = debugASMPlugins;
        }
    }

    //checks if the state & fluid match the criteria set by the cfg
    public static class ConfigPredicate implements BiPredicate<IBlockState, Fluid>
    {
        @Nonnull protected final Block block;
        @Nonnull protected final Map<Integer, Boolean> validMeta;
        @Nonnull protected final Map<Fluid, Boolean> validFluids;

        public ConfigPredicate(@Nonnull Block block, @Nonnull int[] validMeta, @Nonnull Fluid[] validFluids) {
            this.block = block;
            this.validMeta = new HashMap<>();
            this.validFluids = new HashMap<>();

            for(int meta : validMeta) this.validMeta.put(meta, true);
            for(Fluid fluid : validFluids) this.validFluids.put(fluid, true);
        }

        @Override
        public boolean test(@Nonnull IBlockState stateIn, @Nullable Fluid fluidIn) {
            //check fluidIn
            if(fluidIn != null && !validFluids.isEmpty() && !validFluids.containsKey(fluidIn))
                return false;

            //check stateIn
            return validMeta.isEmpty() || validMeta.containsKey(block.getMetaFromState(stateIn));
        }
    }

    //exists to allow the config to be loaded early
    public static class ConfigPredicateBuilder
    {
        @Nonnull protected final int[] validMeta;
        @Nonnull protected final String[] validFluidNames;
        @Nonnull protected final String blockName;

        public ConfigPredicateBuilder(@Nonnull String blockName, @Nonnull int[] validMeta, @Nonnull String[] validFluidNames) {
            this.validMeta = validMeta;
            this.validFluidNames = validFluidNames;
            this.blockName = blockName;
        }

        public ConfigPredicate build() {
            final @Nullable Block block = Block.getBlockFromName(blockName);
            if(block == null) throw new JsonParseException("Fluidlogged API Config: Unable to parse block from blockId: " + blockName);

            final Set<Fluid> validFluids = new HashSet<>();
            for(String fluidName : validFluidNames) {
                @Nullable Block fluidBlock = Block.getBlockFromName(fluidName);
                @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(fluidBlock);

                if(fluid != null && FluidloggedUtils.isFluidloggableFluid(fluidBlock.getDefaultState(), false)) validFluids.add(fluid);
                else throw new JsonParseException("Fluidlogged API Config: Unable to parse fluid from fluids: " + fluidName);
            }

            return new ConfigPredicate(block, validMeta, validFluids.toArray(new Fluid[0]));
        }
    }

    //gson
    public static class Deserializer implements JsonDeserializer<ConfigPredicateBuilder>
    {
        @Nonnull
        @Override
        public ConfigPredicateBuilder deserialize(@Nonnull JsonElement json, @Nullable Type typeOfT, @Nullable JsonDeserializationContext context) throws JsonParseException {
            try {
                final NBTTagCompound nbt = JsonToNBT.getTagFromJson(json.toString());
                if(nbt.hasKey("blockId", Constants.NBT.TAG_STRING)) {
                    final String blockName = nbt.getString("blockId");
                    final Set<String> validFluidNames = new HashSet<>();
                    int[] validMeta = new int[0];

                    //get state meta
                    if(nbt.hasKey("metadata", Constants.NBT.TAG_INT_ARRAY)) {
                        validMeta = nbt.getIntArray("metadata");
                    }

                    //get fluids
                    if(nbt.hasKey("fluids", Constants.NBT.TAG_LIST)) {
                        final NBTTagList list = nbt.getTagList("fluids", Constants.NBT.TAG_STRING);
                        for(int i = 0; i < list.tagCount(); i++) validFluidNames.add(list.getStringTagAt(i));
                    }

                    return new ConfigPredicateBuilder(blockName, validMeta, validFluidNames.toArray(new String[0]));
                }

                //no blockId specified
                else throw new JsonParseException("Fluidlogged API Config: blockId argument not found!");
            }

            //invalid json, probably
            catch(NBTException e) { throw new JsonParseException(e); }
        }
    }
}
