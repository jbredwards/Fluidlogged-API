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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix all fluid-related rain collisions
 * @author jbred
 *
 */
public final class PluginParticleRain implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_189213_a" : "onUpdate"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * onUpdate: (changes are around line 65)
         * Old code:
         * if (iblockstate.getBlock() instanceof BlockLiquid)
         * {
         *     ...
         * }
         *
         * New code:
         * //disable built-in liquid check
         * if (false)
         * {
         *     ...
         * }
         */
        if(insn.getOpcode() == INSTANCEOF) {
            instructions.insert(insn, new InsnNode(ICONST_0));
            removeFrom(instructions, insn, -2);
        }
        /*
         * onUpdate: (changes are around line 71)
         * Old code:
         * d0 = iblockstate.getBoundingBox(this.world, blockpos).maxY;
         *
         * New code:
         * //rain particles collide at the proper height
         * d0 = PluginEntityRenderer.Hooks(iblockstate, this.world, blockpos).maxY;
         */
        else if(checkMethod(insn, obfuscated ? "func_185900_c" : "getBoundingBox")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/client/PluginEntityRenderer$Hooks", "fixRainCollision", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/util/math/AxisAlignedBB;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
