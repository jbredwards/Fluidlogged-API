/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import net.minecraft.entity.player.EntityPlayer;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * disable sprint while in water
 * @author jbred
 *
 */
public final class PluginEntityPlayerSP implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_70636_d" : "onLivingUpdate"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * onLivingUpdate: (changes are around line 1040)
         * Old code:
         * if (this.capabilities.allowFlying)
         * {
         *     ...
         * }
         *
         * New code:
         * // disable sprint while in water
         * if (Hooks.canFlyDisableSprint(this.capabilities.allowFlying, this))
         * {
         *     ...
         * }
         */
        if(checkField(insn, obfuscated ? "field_75101_c" : "allowFlying") && !(getPrevious(insn, 3) instanceof JumpInsnNode)) {
            instructions.insert(insn, genMethodNode("canFlyDisableSprint", "(ZLnet/minecraft/entity/player/EntityPlayer;)Z"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canFlyDisableSprint(final boolean allowFlying, @Nonnull final EntityPlayer player) {
            if(!FluidloggedAPI.isAquaAcrobatics && !player.capabilities.isFlying && player.isInWater()) player.setSprinting(false);
            return allowFlying;
        }
    }
}
