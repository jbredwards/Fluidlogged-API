/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;

import javax.annotation.Nonnull;

/**
 * store a level-to-FluidState lookup array in fluid block state containers
 * @author jbred
 *
 */
public final class PluginBlockStateContainer implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "fluidStates", "[[Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null));
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/ILevelFluidStateLookup");
        /*
         * Accessor:
         * New code:
         * // getter for fluidStates
         * @ASMGenerated
         * public FluidState[][] getFluidStateLookup()
         * {
         *     return this.fluidStates;
         * }
         */
        addMethod(classNode, "getFluidStateLookup", "()[[Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/state/BlockStateContainer", "fluidStates", "[[Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });
        /*
         * Accessor:
         * New code:
         * // setter for fluidStates
         * @ASMGenerated
         * public void setFluidStateLookup(@Nonnull FluidState[][] fluidStates)
         * {
         *     this.fluidStates = fluidStates;
         * }
         */
        addMethod(classNode, "setFluidStateLookup", "([[Lgit/jbredwards/fluidlogged_api/api/util/FluidState;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/state/BlockStateContainer", "fluidStates", "[[Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });

        return false;
    }
}
