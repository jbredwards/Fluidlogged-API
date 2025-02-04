/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.buildcraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * make buildcraft's BlockUtil fluid utility methods FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginBlockUtil implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("getFluidWithFlowing"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * getFluidWithFlowing:
         * Old code:
         * IBlockState blockState = world.getBlockState(pos);
         *
         * New code:
         * // account for FluidStates
         * IBlockState blockState = FluidloggedUtils.getFluidOrReal(world, pos);
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * isFullFluidBlock:
         * New code:
         * // account for FluidStates
         * @ASMOverwrite
         * public static boolean isFullFluidBlock(World world, BlockPos pos)
         * {
         *     return FluidloggedUtils.getFluidState(world, pos).isSource();
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, "isFullFluidBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", false);
            generator.visitMethodInsn(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "isSource", "()Z", false);
        });
        /*
         * isFullFluidBlock:
         * New code:
         * // account for FluidStates
         * @ASMOverwrite
         * public static boolean isFullFluidBlock(IBlockState state, World world, BlockPos pos)
         * {
         *     return FluidloggedUtils.getFluidState(world, pos, state).isSource();
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, "isFullFluidBlock", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 0);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", false);
            generator.visitMethodInsn(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "isSource", "()Z", false);
        });

        return true;
    }
}
