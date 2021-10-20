package git.jbredwards.fluidlogged_api.api.util;

import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.BlockFluidClassic;
import net.minecraftforge.fluids.BlockFluidFinite;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

import javax.annotation.Nonnull;
import java.lang.reflect.Field;

/**
 * makes some private & protected things accessible
 * @author jbred
 *
 */
public enum FluidloggedAccessorUtils
{
    ;

    //private or protected fields
    public static final Field canCreateSources    = ObfuscationReflectionHelper.findField(BlockFluidClassic.class, "canCreateSources");
    public static final Field quantaPerBlockFloat = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaPerBlockFloat");
    public static final Field quantaPerBlock      = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaPerBlock");
    public static final Field quantaFraction      = ObfuscationReflectionHelper.findField(BlockFluidBase.class, "quantaFraction");
    static {
        canCreateSources.setAccessible(true);
        quantaPerBlockFloat.setAccessible(true);
        quantaPerBlock.setAccessible(true);
        quantaFraction.setAccessible(true);
    }

    public static boolean canCreateSources(@Nonnull Fluid fluid) {
        final Block block = fluid.getBlock();

        try { return canCreateSources.getBoolean(block); }
        catch(Throwable t) {
            if(block instanceof BlockFluidFinite) return false;
            else return block != null && block.getDefaultState().getMaterial() == Material.WATER;
        }
    }

    public static float quantaPerBlockFloat(@Nonnull Fluid fluid) {
        try { return quantaPerBlockFloat.getFloat(fluid.getBlock()); }
        catch(Throwable t) { return 8; }
    }

    public static int quantaPerBlock(@Nonnull Fluid fluid) {
        try { return quantaPerBlock.getInt(fluid.getBlock()); }
        catch(Throwable t) { return 8; }
    }

    public static float quantaFraction(@Nonnull Fluid fluid) {
        try { return quantaFraction.getFloat(fluid.getBlock()); }
        catch(Throwable t) { return 8f / 9; }
    }
}
