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

package git.jbredwards.fluidlogged_api.mod.asm.transformers;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.PluginBlockWall;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Many modded wall blocks don't extend BlockWall and instead copy all the code from BlockWall.
 * This transformer attempts to find all modded wall block classes, and applies the {@link PluginBlockWall} fix
 * & the {@link git.jbredwards.fluidlogged_api.mod.asm.iface.IModdedWall} interface (so they can be easily
 * identified as walls, used by this mod's builtin whitelist).
 * @author jbred
 *
 */
public final class TransformerModdedWalls implements IClassTransformer, IASMPlugin
{
    @Nullable
    @Override
    public byte[] transform(@Nullable final String name, @Nullable final String transformedName, @Nullable final byte[] basicClass) {
        return basicClass != null && transformedName != null && transformedName.contains("Wall") ? transform(basicClass, !FMLLaunchHandler.isDeobfuscatedEnvironment()) : basicClass;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        if(classNode.methods.stream().anyMatch(method -> checkMethod(method, "getAABBIndex", "(Lnet/minecraft/block/state/IBlockState;)I"))) {
            IASMPlugin.setActivePlugin("Fluidlogged API Plugin");
            classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/IModdedWall");
            new PluginBlockWall().transformNode(classNode, obfuscated);
            IASMPlugin.resetActivePlugin();
        }

        return false;
    }

    @Override
    public boolean shouldInformConsole() { return false; /* handled by PluginBlockWall */ }
}
