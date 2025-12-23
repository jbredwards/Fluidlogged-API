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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.bedrockores;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.util.BlockRenderLayer;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.fml.common.FMLCommonHandler;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fix issue#276
 * @author jbred
 *
 */
public final class PluginBedrockOre implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_176221_a" : "getActualState"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * getActualState:
         * Old code:
         * if ((world instanceof World && !((World) world).isRemote) || MinecraftForgeClient.getRenderLayer() == null)
         * {
         *     ...
         * }
         *
         * New code:
         * // Prevent loading client-side class on server.
         * if ((world instanceof World && !((World) world).isRemote) || Hooks.getRenderLayer() == null)
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, "getRenderLayer")) {
            ((MethodInsnNode)insn).owner = getHookClass();
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static BlockRenderLayer getRenderLayer() {
            return FMLCommonHandler.instance().getSide().isClient() ? MinecraftForgeClient.getRenderLayer() : null;
        }
    }
}
