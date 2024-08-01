/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.endercore;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fix fluid collisions and make enderio's block access wrapper FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginEnderCore implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("onRenderBlockOverlay"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * onRenderBlockOverlay:
         * Old code:
         * Block block = player.world.getBlockState(blockpos).getBlock();
         *
         * New code:
         * // Account for FluidStates
         * Block block = FluidloggedUtils.getFluidOrReal(player.world, blockpos).getBlock();
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
        if(classNode.name.endsWith("BlockFluidEnder")) overrideMethod(classNode, method -> method.name.equals("isEntityInsideMaterial"),
            "isEntityInsideMaterial", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(DLOAD, 5);
                generator.visitVarInsn(ALOAD, 7);
                generator.visitVarInsn(ILOAD, 8);
            }
        );
        else if(classNode.name.endsWith("FluidVisualsHandler")) return true;
        else {
            classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/world/IBlockAccessWrapper");
            addMethod(classNode, "getWrapped", "()Lnet/minecraft/world/IBlockAccess;", null, null, generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "com/enderio/core/common/util/IBlockAccessWrapper", "wrapped", "Lnet/minecraft/world/IBlockAccess;");
            });
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static Boolean isEntityInsideMaterial(@Nonnull final IBlockAccess world, @Nonnull final BlockPos blockpos, @Nonnull final IBlockState iblockstate, @Nonnull final Entity entity, final double yToTest, @Nonnull final Material materialIn, final boolean testingHead) {
            return FluidCollisionHandler.isEntityInsideMaterial(world, blockpos, iblockstate, entity, yToTest, materialIn == Material.WATER ? iblockstate.getMaterial() : materialIn, testingHead);
        }
    }
}
