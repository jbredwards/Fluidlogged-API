package git.jbredwards.fluidlogged_api.common.util;

import net.minecraft.block.Block;
import net.minecraft.block.BlockBush;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.BlockFluidClassic;
import net.minecraftforge.fluids.BlockFluidFinite;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * makes some private & protected things accessible
 * @author jbred
 *
 */
public enum AccessorUtils
{
    ;

    //private or protected fields
    @Nonnull public static final Field canCreateSources    = ObfuscationReflectionHelper.findField(BlockFluidClassic.class, "canCreateSources");
    @Nonnull public static final Field quantaPerBlockFloat = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaPerBlockFloat");
    @Nonnull public static final Field quantaPerBlock      = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaPerBlock");
    @Nonnull public static final Field quantaFraction      = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaFraction");
    @Nonnull public static final Field densityDir          = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "densityDir");
    //private or protected methods
    @Nonnull public static final Method canSustainBush = ObfuscationReflectionHelper.findMethod(BlockBush.class, "func_185514_i", boolean.class, IBlockState.class);

    //make everything accessible
    static {
        //fields
        canCreateSources.setAccessible(true);
        quantaPerBlockFloat.setAccessible(true);
        quantaPerBlock.setAccessible(true);
        quantaFraction.setAccessible(true);
        densityDir.setAccessible(true);
        //methods
        canSustainBush.setAccessible(true);
    }

    public static boolean canCreateSources(@Nullable Block fluid) {
        try { return canCreateSources.getBoolean(fluid); }
        catch(Throwable t) {
            if(fluid instanceof BlockFluidFinite) return false;
            else return fluid != null && fluid.getDefaultState().getMaterial() == Material.WATER;
        }
    }

    public static float quantaPerBlockFloat(@Nullable Block fluid) {
        try { return quantaPerBlockFloat.getFloat(fluid); }
        catch(Throwable t) { return 8; }
    }

    public static int quantaPerBlock(@Nullable Block fluid) {
        try { return quantaPerBlock.getInt(fluid); }
        catch(Throwable t) { return 8; }
    }

    public static float quantaFraction(@Nullable Block fluid) {
        try { return quantaFraction.getFloat(fluid); }
        catch(Throwable t) { return 8f / 9; }
    }

    public static int densityDir(@Nullable Block fluid) {
        try { return densityDir.getInt(fluid); }
        catch(Throwable t) { return -1; }
    }

    public static boolean canSustainBush(@Nonnull BlockBush bush, @Nonnull IBlockState state) {
        try { return (boolean)canSustainBush.invoke(bush, state); }
        catch (InvocationTargetException | IllegalAccessException e) { return false; }
    }
}
