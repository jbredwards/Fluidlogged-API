package git.jbredwards.fluidlogged_api.mod.common.config;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.ASMHooks;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.ASMNatives;
import git.jbredwards.fluidlogged_api.mod.common.config.js.JSFluid;
import git.jbredwards.fluidlogged_api.mod.common.config.js.JSPos;
import git.jbredwards.fluidlogged_api.mod.common.config.js.JSState;
import git.jbredwards.fluidlogged_api.mod.common.config.js.JSWorld;
import it.unimi.dsi.fastutil.ints.Int2BooleanMap;
import it.unimi.dsi.fastutil.ints.Int2BooleanOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2BooleanMap;
import it.unimi.dsi.fastutil.objects.Object2BooleanOpenHashMap;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.Loader;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;

/**
 *
 * @author jbred
 *
 */
public final class ConfigHandler
{
    @Nonnull static final ScriptEngine ENGINE = new ScriptEngineManager(null).getEngineByName("JavaScript");
    //stores the set of invalid blocks when the config is loaded, if this isn't empty when the config is done, print the set & crash
    @Nonnull static final Set<String> INVALID_BLOCKS = new HashSet<>();
    //stores the set of invalid fluids when the config is loaded, if this isn't empty when the config is done, print the set & crash
    @Nonnull static final Set<String> INVALID_FLUIDS = new HashSet<>();

    @SuppressWarnings("unchecked")
    static final class API
    {
        public static void whitelist(@Nonnull String name) { whitelist(name, ConfigFluidloggable.ALWAYS_TRUE); }
        public static void whitelist(@Nonnull String name, @Nullable Object o) {
            final @Nullable Block block = Block.getBlockFromName(name);
            if(block == null || !(o instanceof Predicate)) INVALID_BLOCKS.add(name);
            else ASMNatives.setFluidloggable(block, event -> fromObject(o).test(event)
                    || ASMNatives.getFluidloggable(block).test(event));
        }

        public static void blacklist(@Nonnull String name) { blacklist(name, ConfigFluidloggable.ALWAYS_TRUE); }
        public static void blacklist(@Nonnull String name, @Nullable Object o) {
            final @Nullable Block block = Block.getBlockFromName(name);
            if(block == null || !(o instanceof Predicate)) INVALID_BLOCKS.add(name);
            else ASMNatives.setFluidloggable(block, event -> !fromObject(o).test(event)
                    && ASMNatives.getFluidloggable(block).test(event));
        }

        public static void override(@Nonnull String name) { override(name, ConfigFluidloggable.ALWAYS_TRUE); }
        public static void override(@Nonnull String name, @Nullable Object o) {
            final @Nullable Block block = Block.getBlockFromName(name);
            if(block == null || !(o instanceof Predicate)) INVALID_BLOCKS.add(name);
            else ASMNatives.setFluidloggable(block, fromObject(o));
        }

        public static void reset(@Nonnull String name) {
            final @Nullable Block block = Block.getBlockFromName(name);
            if(block == null) INVALID_BLOCKS.add(name);
            else ASMHooks.initInternalFluidloggable(block);
        }

        public static boolean isModLoaded(@Nonnull String modId) { return Loader.isModLoaded(modId); }

        //internal
        @Nonnull
        static Predicate<ConfigFluidloggable.Event> fromObject(@Nullable Object o) {
            if(o == null) return event -> false;
            else if(o instanceof Predicate) return (Predicate<ConfigFluidloggable.Event>)o;

            //read from js
            else for(Method method : o.getClass().getDeclaredMethods()) {
                if(method.getParameterCount() == 1) {
                    method.setAccessible(true);
                    return event -> {
                        try { return (Boolean)method.invoke(null, event); }
                        catch (IllegalAccessException | InvocationTargetException e) { e.printStackTrace(); }
                        return false;
                    };
                }
            }

            //====================
            //build legacy adapter
            //====================

            //scan for metadata arg
            int[] metadata = new int[0];
            try {
                final Field field = o.getClass().getDeclaredField("metadata");
                field.setAccessible(true);

                if(field.getType().equals(int.class)) metadata = new int[] { field.getInt(o) };
                else if(field.getType().equals(int[].class)) metadata = (int[])field.get(o);

            }
            //no metadata arg, ignore
            catch (NoSuchFieldException | IllegalAccessException ignored) {}

            //scan for fluids arg
            String[] fluids = new String[0];
            try {
                final Field field = o.getClass().getDeclaredField("fluids");
                field.setAccessible(true);

                if(field.getType().equals(String.class)) fluids = new String[] { (String)field.get(o) };
                else if(field.getType().equals(String[].class)) fluids = (String[])field.get(o);
            }
            //no fluids arg, ignore
            catch (NoSuchFieldException | IllegalAccessException ignored) {}

            //if no metadata and no fluids, the condition is always true
            if(metadata.length == 0 && fluids.length == 0) return ConfigFluidloggable.ALWAYS_TRUE;
            else return new LegacyPredicate(metadata, fluids);
        }
    }

    @FunctionalInterface
    public interface ConfigFluidloggable extends Predicate<ConfigFluidloggable.Event>
    {
        @Nonnull ConfigFluidloggable ALWAYS_TRUE = event -> true;

        class Event
        {
            @Nonnull public final JSWorld world;
            @Nonnull public final JSPos pos;
            @Nonnull public final JSState state;
            @Nonnull public final JSFluid fluid;

            public Event(@Nonnull World worldIn, @Nonnull BlockPos posIn, @Nonnull IBlockState stateIn, @Nonnull FluidState fluidIn) {
                world = new JSWorld(worldIn);
                pos = new JSPos(posIn);
                state = new JSState(stateIn);
                fluid = new JSFluid(fluidIn);
            }

            @Nonnull
            public JSWorld getWorld() { return world; }

            @Nonnull
            public JSPos getPos() { return pos; }

            @Nonnull
            public JSState getState() { return state; }

            @Nonnull
            public JSFluid getFluid() { return fluid; }
        }
    }

    static final class LegacyPredicate implements Predicate<ConfigFluidloggable.Event>
    {
        @Nonnull final Int2BooleanMap metadata = new Int2BooleanOpenHashMap();
        @Nonnull final Object2BooleanMap<Fluid> fluids = new Object2BooleanOpenHashMap<>();

        public LegacyPredicate(int[] metadata, String[] fluids) {
            for(int meta : metadata) this.metadata.put(meta, true);
            for(@Nullable String fluidName : fluids) {
                if(fluidName != null) {
                    @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(Block.getBlockFromName(fluidName));
                    if(fluid == null) fluid = FluidRegistry.getFluid(fluidName);

                    //no fluid was found for the name provided, add to the set
                    if(fluid == null) INVALID_FLUIDS.add(fluidName);
                    else this.fluids.put(fluid, true);
                }
            }
        }

        @Override
        public boolean test(@Nonnull ConfigFluidloggable.Event event) {
            //check fluid
            if(!event.fluid.isEmpty() && !fluids.isEmpty() && !fluids.containsKey(event.fluid.getJavaFluid()))
                return false;

            //check stateIn
            return metadata.isEmpty() || metadata.containsKey(event.state.getMeta());
        }
    }
}
