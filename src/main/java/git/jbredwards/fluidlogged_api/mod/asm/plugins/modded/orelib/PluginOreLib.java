/*
 * Copyright (C) <2026 to Present> <jbredwards>
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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.orelib;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.world.ICubeData;
import git.jbredwards.fluidlogged_api.api.world.ICubeDataProvider;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * allow OreLib's IBlockAccessEx to read FluidStates
 * @author jbred
 *
 */
public final class PluginOreLib implements IASMPlugin
{
    final boolean useWorld;
    public PluginOreLib(boolean useWorldIn) { useWorld = useWorldIn; }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/ICubeDataProvider");
        /*
         * New code:
         * //allow OreLib's IBlockAccessEx to read FluidStates
         * @ASMGenerated
         * public git.jbredwards.fluidlogged_api.api.world.ICubeData getCubeData(int chunkX, int chunkY, int chunkZ)
         * {
         *     return Hooks.getCubeData(this.cache, chunkX, chunkY, chunkZ);
         * }
         */
        addMethod(classNode, "getCubeData", "(III)Lgit/jbredwards/fluidlogged_api/api/world/ICubeData;",
            "getCubeData", "(Lgit/jbredwards/fluidlogged_api/api/world/ICubeDataProvider;III)Lgit/jbredwards/fluidlogged_api/api/world/ICubeData;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, classNode.name, useWorld ? "world" : "cache", useWorld ? "Lnet/minecraft/world/World;" : "Lnet/minecraft/world/ChunkCache;");
                generator.visitVarInsn(ILOAD, 1);
                generator.visitVarInsn(ILOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static ICubeData getCubeData(@Nullable final ICubeDataProvider provider, final int chunkX, final int chunkY, final int chunkZ) {
            return provider == null ? ICubeData.EMPTY : provider.getCubeData(chunkX, chunkY, chunkZ);
        }
    }
}
