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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.biome.BiomeColorHelper;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * add water's biome colors to its fluid class
 * @author jbred
 *
 */
public final class PluginFluidWater implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * New code:
         * //use biome here if server, or biome blend if client
         * @ASMGenerated
         * public int getColor(World world, BlockPos pos)
         * {
         *     return Hooks.getWaterColorAt(world, pos);
         * }
         */
        addMethod(classNode, "getColor", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I",
            "getWaterColorAt", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static int getWaterColorAt(@Nonnull World world, @Nonnull BlockPos pos) {
            return world.isRemote ? BiomeColorHelper.getWaterColorAtPos(world, pos) : world.getBiome(pos).getWaterColorMultiplier();
        }
    }
}
