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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.spongeforge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.PluginWorldServer;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * spongeforge no longer mixins into conflicting methods
 * @author jbred
 *
 */
public final class PluginSpongeForge implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.methods.removeIf(method
                -> method.name.equals("spongeImpl$onEntityCollideWithBlockState")
                || method.name.equals("impl$CheckForLiquidMixing")
                || method.name.equals("impl$throwModifyForLavaToStone")
                || method.name.equals("impl$CheckEventsBeforeSpreadingFire")
                || method.name.equals("asyncLighting$onRelightChecksGetBlockState")
                || method.name.equals("onCheckDisplaceIfPreAlreadyCancelled"));

        return false;
    }
}
