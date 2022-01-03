package git.jbredwards.fluidlogged_api.common.config;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import net.minecraft.block.Block;
import net.minecraft.block.properties.IProperty;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.*;
import net.minecraft.util.EnumActionResult;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiPredicate;
import java.util.function.Predicate;

/**
 *
 * @author jbred
 *
 */
public enum FluidloggedConfig
{
    ;

    public static final Map<Block, BiPredicate<IBlockState, Fluid>> WHITELIST = new HashMap<>();
    public static final Map<Block, BiPredicate<IBlockState, Fluid>> BLACKLIST = new HashMap<>();
    public static boolean applyDefaults = true;

    public static EnumActionResult isStateFluidloggable(@Nonnull IBlockState state, @Nullable Fluid fluid) {
        //check whitelist
        final @Nullable BiPredicate<IBlockState, Fluid> whiteList = WHITELIST.get(state.getBlock());
        if(whiteList != null && whiteList.test(state, fluid)) return EnumActionResult.SUCCESS;
        //save some time by not checking blacklist if defaults aren't enabled in the first place
        if(!applyDefaults) return EnumActionResult.FAIL;
        //check blacklist
        final @Nullable BiPredicate<IBlockState, Fluid> blackList = BLACKLIST.get(state.getBlock());
        if(blackList != null && blackList.test(state, fluid)) return EnumActionResult.FAIL;
        //default
        return EnumActionResult.PASS;
    }

    public static class Deserializer implements JsonDeserializer<BiPredicate<IBlockState, Fluid>>
    {
        @SuppressWarnings("deprecation")
        @Nonnull
        @Override
        public BiPredicate<IBlockState, Fluid> deserialize(@Nonnull JsonElement json, @Nullable Type typeOfT, @Nullable JsonDeserializationContext context) throws JsonParseException {
            try {
                final NBTTagCompound nbt = JsonToNBT.getTagFromJson(json.toString());
                if(nbt.hasKey("blockID", Constants.NBT.TAG_STRING)) {
                    final @Nullable Block block = Block.getBlockFromName(nbt.getString("blockID"));
                    if(block == null) throw new JsonParseException("Fluidlogged API Config: Unable to parse block: " + nbt.getString("blockID"));
                    //block is valid hooray
                    else {
                        final List<Predicate<IBlockState>> stateCriteria = new ArrayList<>();
                        final List<Predicate<Fluid>>       fluidCriteria = new ArrayList<>();

                        //check state criteria
                        if(nbt.hasKey("stateCriteria", Constants.NBT.TAG_INT_ARRAY)) {
                            for(int meta : nbt.getIntArray("stateCriteria")) {
                                Map<IProperty<?>, Comparable<?>> props = block.getStateFromMeta(meta).getProperties();
                                stateCriteria.add(stateIn -> props.equals(stateIn.getProperties()));
                            }
                        }

                        //check fluid criteria
                        if(nbt.hasKey("fluidCriteria", Constants.NBT.TAG_LIST)) {
                            final NBTTagList list = nbt.getTagList("fluidCriteria", Constants.NBT.TAG_STRING);
                            for(int i = 0; i < list.tagCount(); i++) {
                                @Nullable Fluid fluid = FluidRegistry.getFluid(list.getStringTagAt(i));
                                if(fluid == null) throw new JsonParseException("Fluidlogged API Config: Unable to parse fluid: " + list.getStringTagAt(i));
                                else fluidCriteria.add(fluid::equals);
                            }
                        }

                        //checks if the state & fluid match the criteria set by the cfg
                        return (stateIn, fluidIn) -> {
                            //check state
                            boolean stateFlag = stateCriteria.isEmpty();
                            if(!stateFlag) for(Predicate<IBlockState> flag : stateCriteria) {
                                if(flag.test(stateIn)) {
                                    stateFlag = true;
                                    break;
                                }
                            }
                            //save some time by returning false if state isn't valid
                            if(!stateFlag) return false;
                            //don't check fluid if empty
                            if(fluidIn == null) return true;
                            //check fluid
                            if(!fluidCriteria.isEmpty()) for(Predicate<Fluid> flag : fluidCriteria) {
                                if(flag.test(fluidIn)) return true;
                            }

                            //default
                            return false;
                        };
                    }
                }
                else throw new JsonParseException("Fluidlogged API Config: no block set!");
            }
            //oops
            catch (NBTException e) { throw new JsonParseException(e); }
        }
    }
}
