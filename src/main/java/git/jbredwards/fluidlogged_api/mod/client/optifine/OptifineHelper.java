/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.client.optifine;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.optifine.PluginIResolvable;
import net.minecraft.init.Biomes;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.biome.BiomeColorHelper;
import net.minecraftforge.fml.relauncher.ReflectionHelper;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Support for Optifine's custom water colors
 * @author jbred
 *
 */
public final class OptifineHelper
{
    static final Field waterColors_Field;
    static final Method getColor_Method;
    static final Method isSwampColors_Method;
    static {
        try {
            //noinspection deprecation
            waterColors_Field = ReflectionHelper.findField(Class.forName("net.optifine.CustomColors"), "waterColors");
            //noinspection deprecation
            getColor_Method = ReflectionHelper.findMethod(Class.forName("net.optifine.CustomColormap"), "getColor", null, Biome.class, BlockPos.class);
            //noinspection deprecation
            isSwampColors_Method = ReflectionHelper.findMethod(Class.forName("Config"), "isSwampColors", null);
        }

        //should never be thrown
        catch(final ClassNotFoundException e) { throw new RuntimeException(e); }
    }

    public static void setWaterColorHelper() {
        BiomeColorHelper.WATER_COLOR = (biome, blockPos) -> {
            try {
                //OF has a setting to toggle swampland colors. If it's set to not support swamp colors, set to plains (this is what OF does)
                if(biome == Biomes.SWAMPLAND && !(boolean)isSwampColors_Method.invoke(null)) biome = Biomes.PLAINS;

                final Object waterColors = waterColors_Field.get(null);
                if(waterColors == null) return biome.getWaterColor(); //current resource pack has no water color change

                final int colorOF = (int)getColor_Method.invoke(waterColors, biome, blockPos);
                return colorOF != -1 ? colorOF : biome.getWaterColor();
            }

            //should never be thrown
            catch(final IllegalAccessException | InvocationTargetException e) { throw new RuntimeException(e); }
        };
    }

    public static void onLoadComplete() { PluginIResolvable.Hooks.resolve(); }
}
