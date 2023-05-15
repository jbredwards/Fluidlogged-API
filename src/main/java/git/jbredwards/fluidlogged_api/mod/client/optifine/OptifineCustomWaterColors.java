package git.jbredwards.fluidlogged_api.mod.client.optifine;

import net.minecraft.init.Biomes;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.biome.BiomeColorHelper;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Support for Optifine's custom water colors
 * @author jbred
 *
 */
public final class OptifineCustomWaterColors
{
    static final Field waterColors_Field;
    static final Method getColor_Method;
    static final Method isSwampColors_Method;
    static {
        try {
            waterColors_Field = ObfuscationReflectionHelper.findField(Class.forName("net.optifine.CustomColors"), "waterColors");
            getColor_Method = ObfuscationReflectionHelper.findMethod(Class.forName("net.optifine.CustomColormap"), "getColor", int.class, Biome.class, BlockPos.class);
            isSwampColors_Method = ObfuscationReflectionHelper.findMethod(Class.forName("Config"), "isSwampColors", boolean.class);
        }

        //should never be thrown
        catch(final ClassNotFoundException e) { throw new RuntimeException(e); }
    }

    public static void setWaterColorHelper() {
        BiomeColorHelper.WATER_COLOR = (biome, blockPos) -> {
            try {
                //OF has a setting to toggle swampland colors, if it's set to not support swamp colors, set to plains (this is what OF does)
                if(biome == Biomes.SWAMPLAND && !(boolean)isSwampColors_Method.invoke(null)) biome = Biomes.PLAINS;

                final Object waterColors = waterColors_Field.get(null);
                if(waterColors == null) return biome.getWaterColor(); //current resource pack has no water color change

                final int colorOF = (int)getColor_Method.invoke(waterColors, biome, blockPos);
                return colorOF != -1 ? colorOF : biome.getWaterColor();
            }

            //should never be thrown
            catch(IllegalAccessException | InvocationTargetException e) { throw new RuntimeException(e); }
        };
    }
}
