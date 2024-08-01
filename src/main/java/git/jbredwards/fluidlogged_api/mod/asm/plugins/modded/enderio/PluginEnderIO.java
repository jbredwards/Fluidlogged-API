/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.enderio;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * account for FluidStates and fix fluid collision
 * @author jbred
 *
 */
public final class PluginEnderIO implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("canMakeSnow"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * canMakeSnow:
         * Old code:
         * Block neighborBlock = neighborState.getBlock();
         *
         * New code:
         * // Account for FluidStates
         * Block neighborBlock = FluidloggedUtils.getFluidOrReal(world, neighborPos, neighborState).getBlock();
         */
        if(checkMethod(insn, obfuscated ? "func_177230_c" : "getBlock")) {
            instructions.insertBefore(insn.getPrevious(), new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn.getPrevious(), new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        if(classNode.name.endsWith("FireWater")) {
            overrideMethod(classNode, method -> method.name.equals("isEntityInsideMaterial"),
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

            return false;
        }

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static Boolean isEntityInsideMaterial(@Nonnull final IBlockAccess world, @Nonnull final BlockPos blockpos, @Nonnull final IBlockState iblockstate, @Nonnull final Entity entity, final double yToTest, @Nonnull final Material materialIn, final boolean testingHead) {
            return FluidCollisionHandler.isEntityInsideMaterial(world, blockpos, iblockstate, entity, yToTest, materialIn == Material.LAVA ? iblockstate.getMaterial() : materialIn, testingHead);
        }
    }
}
