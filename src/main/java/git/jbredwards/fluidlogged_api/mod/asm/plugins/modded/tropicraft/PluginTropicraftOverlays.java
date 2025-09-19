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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tropicraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * account for FluidStates and improved fluid collisions
 * @author jbred
 *
 */
public final class PluginTropicraftOverlays implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("onBlockOverlay"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * onBlockOverlay:
         * Old code:
         * IBlockState atPos = event.getPlayer().getEntityWorld().getBlockState(blockpos);
         *
         * New code:
         * // account for FluidStates
         * IBlockState atPos = ActiveRenderInfo.getBlockStateAtEntityViewpoint(event.getPlayer().getEntityWorld(), event.getPlayer(), event.getRenderPartialTicks());
         */
        if(checkMethod(insn.getNext(), obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/client/event/RenderBlockOverlayEvent", "getPlayer", "()Lnet/minecraft/entity/player/EntityPlayer;", false));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/client/event/RenderBlockOverlayEvent", "getRenderPartialTicks", "()F", false));
            instructions.insertBefore(insn, genMethodNode("net/minecraft/client/renderer/ActiveRenderInfo", obfuscated ? "func_186703_a" : "getBlockStateAtEntityViewpoint", "(Lnet/minecraft/world/World;Lnet/minecraft/entity/Entity;F)Lnet/minecraft/block/state/IBlockState;"));
            removeFrom(instructions, insn, 1);
            return true;
        }

        return false;
    }
}
