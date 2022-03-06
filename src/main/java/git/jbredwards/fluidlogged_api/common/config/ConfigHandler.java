package git.jbredwards.fluidlogged_api.common.config;

import com.google.common.collect.ImmutableMap;
import com.google.gson.*;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.JsonToNBT;
import net.minecraft.nbt.NBTException;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.EnumActionResult;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import org.apache.commons.io.IOUtils;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Type;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.BiPredicate;

/**
 *
 * @author jbred
 *
 */
public enum ConfigHandler
{
    ;

    private static Map<Block, ConfigPredicate> WHITELIST = ImmutableMap.of();
    private static Map<Block, ConfigPredicate> BLACKLIST = ImmutableMap.of();
    private static boolean applyDefaults = true;

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

    //internal use only!
    @SuppressWarnings("ResultOfMethodCallIgnored")
    public static void initialize() throws IOException {
        final File path = new File("config/jbred/1.12.2");
        path.mkdirs();

        final File cfg = new File(path, "fluidlogged_api.cfg");

        //creates a new cfg file
        if(cfg.createNewFile()) {
            //default config file contents
            final String contents =
                    "#this mod by default allows certain blocks to be fluidlogged\n" +
                    "\"applyDefaults\":true,\n" +
                    "\n" +
                    "#whitelist for adding new fluidloggable blocks (this is in addition to the defaults)\n" +
                    "#info about the format for this can be found on this mod's wiki\n" +
                    "\"whitelist\":[],\n" +
                    "\n" +
                    "#blacklist blocks from the defaults\n" +
                    "#info about the format for this can be found on this mod's wiki\n" +
                    "\"blacklist\":[]";

            //writes the default entries to the new cfg file
            final FileWriter writer = new FileWriter(cfg);
            writer.write(contents);
            writer.close();
        }

        //reads an already existing cfg file
        else {
            final Gson gson = new GsonBuilder().registerTypeAdapter(ConfigPredicate.class, new Deserializer()).create();
            final Config config = gson.fromJson('{'+IOUtils.toString(new FileInputStream(cfg), Charset.defaultCharset())+'}', Config.class);

            applyDefaults = config.applyDefaults;

            final ImmutableMap.Builder<Block, ConfigPredicate> whitelistBuilder = ImmutableMap.builder();
            for(ConfigPredicate predicate : config.whitelist) whitelistBuilder.put(predicate.block, predicate);
            WHITELIST = whitelistBuilder.build();

            final ImmutableMap.Builder<Block, ConfigPredicate> blacklistBuilder = ImmutableMap.builder();
            for(ConfigPredicate predicate : config.blacklist) blacklistBuilder.put(predicate.block, predicate);
            BLACKLIST = blacklistBuilder.build();
        }
    }

    //gson
    public static class Config
    {
        public final boolean applyDefaults;
        public final ConfigPredicate[] whitelist;
        public final ConfigPredicate[] blacklist;

        public Config(boolean applyDefaults, ConfigPredicate[] whitelist, ConfigPredicate[] blacklist) {
            this.applyDefaults = applyDefaults;
            this.whitelist = whitelist;
            this.blacklist = blacklist;
        }
    }

    //checks if the state & fluid match the criteria set by the cfg
    public static class ConfigPredicate implements BiPredicate<IBlockState, Fluid>
    {
        @Nonnull protected final int[] validMeta;
        @Nonnull protected final Fluid[] validFluids;
        @Nonnull protected final Block block;

        public ConfigPredicate(@Nonnull Block block, @Nonnull int[] validMeta, @Nonnull Fluid[] validFluids) {
            this.validMeta = validMeta;
            this.validFluids = validFluids;
            this.block = block;
        }

        @Override
        public boolean test(@Nonnull IBlockState stateIn, @Nullable Fluid fluidIn) {
            //check fluidIn
            if(fluidIn != null && validFluids.length != 0) {
                boolean isInvalidFluid = true;
                for(Fluid fluid : validFluids) {
                    if(fluid.equals(fluidIn)) {
                        isInvalidFluid = false;
                        break;
                    }
                }

                if(isInvalidFluid) return false;
            }

            //check stateIn
            if(validMeta.length == 0) return true;
            final int blockMeta = block.getMetaFromState(stateIn);

            for(int meta : validMeta) {
                if(meta == blockMeta) return true;
            }

            //default
            return false;
        }
    }

    //gson
    public static class Deserializer implements JsonDeserializer<ConfigPredicate>
    {
        @Nonnull
        @Override
        public ConfigPredicate deserialize(@Nonnull JsonElement json, @Nullable Type typeOfT, @Nullable JsonDeserializationContext context) throws JsonParseException {
            try {
                final NBTTagCompound nbt = JsonToNBT.getTagFromJson(json.toString());
                if(nbt.hasKey("blockId", Constants.NBT.TAG_STRING)) {
                    final @Nullable Block block = Block.getBlockFromName(nbt.getString("blockId"));
                    if(block == null) throw new JsonParseException("Fluidlogged API Config: Unable to parse block from blockId: " + nbt.getString("blockId"));
                    //block is valid hooray
                    else {
                        final Set<Fluid> validFluids = new HashSet<>();
                        int[] validMeta = new int[0];

                        //get state meta
                        if(nbt.hasKey("metadata", Constants.NBT.TAG_INT_ARRAY)) {
                            validMeta = nbt.getIntArray("metadata");
                        }

                        //get fluids
                        if(nbt.hasKey("fluids", Constants.NBT.TAG_LIST)) {
                            final NBTTagList list = nbt.getTagList("fluids", Constants.NBT.TAG_STRING);
                            for(int i = 0; i < list.tagCount(); i++) {
                                @Nullable Fluid fluid = FluidRegistry.getFluid(list.getStringTagAt(i));
                                
                                if(fluid != null) validFluids.add(fluid);
                                else throw new JsonParseException("Fluidlogged API Config: Unable to parse fluid from validFluids: " + list.getStringTagAt(i));
                            }
                        }

                        return new ConfigPredicate(block, validMeta, validFluids.toArray(new Fluid[0]));
                    }
                }

                //no blockId specified
                else throw new JsonParseException("Fluidlogged API Config: blockId argument not found!");
            }

            //invalid json, probably
            catch(NBTException e) { throw new JsonParseException(e); }
        }
    }
}
