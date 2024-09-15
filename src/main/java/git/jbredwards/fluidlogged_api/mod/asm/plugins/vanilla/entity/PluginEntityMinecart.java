/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.entity.Entity;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * minecarts account for fluids when applying drag
 * @author jbred
 *
 */
public final class PluginEntityMinecart implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_94101_h" : "applyDrag"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * applyDrag: (changes are around line 731)
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * // append fluid drag logic to the end of the method
         * {
         *     ...
         *     Hooks.applyFluidDrag(this);
         * }
         */
        if(insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, genMethodNode("applyFluidDrag", "(Lnet/minecraft/entity/Entity;)V"));
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void applyFluidDrag(@Nonnull final Entity minecart) {
            if(minecart.isInWater() && minecart.isPushedByWater()) {
                minecart.motionX *= 0.8;
                minecart.motionZ *= 0.8;
            }
        }
    }
}
