/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidMixHandler;
import net.minecraft.init.Blocks;
import net.minecraftforge.fluids.FluidRegistry;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.Collections;

/**
 * fix lycanites fluid source logic
 * @author jbred
 *
 */
public final class PluginLycanitesFluidSources extends PluginLycanitesFluidMixing implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("<init>"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Constructor:
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * // Use forge fluid source creation logic
         * {
         *     ...
         *     this.canCreateSources = true;
         * }
         */
        if(insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new InsnNode(ICONST_1));
            instructions.insertBefore(insn, new FieldInsnNode(PUTFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "canCreateSources", "Z"));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        if(classNode.name.endsWith("BlockFluidMoglava")) super.transformClass(classNode, obfuscated);
        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static Collection<FluidMixHandler.MixCondition> getMixConditions() {
            return Collections.singleton(FluidMixHandler.forFluid((source, access, pos, sourcePos, side) ->
                FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(access, pos).getFluid(), FluidRegistry.WATER) ? Blocks.COBBLESTONE.getDefaultState() : null
            ));
        }
    }
}
