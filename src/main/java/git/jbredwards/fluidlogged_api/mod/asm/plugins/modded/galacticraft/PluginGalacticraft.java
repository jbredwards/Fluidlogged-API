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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.IWorldProvider;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import micdoodle8.mods.galacticraft.core.proxy.ClientProxyCore;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fix rendering issues with certain galacticraft fluids
 * @author jbred
 *
 */
public final class PluginGalacticraft implements IASMPlugin
{
    public final boolean isFluidUtil;
    public PluginGalacticraft(boolean isFluidUtilIn) { isFluidUtil = isFluidUtilIn; }

    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("isFlammable"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * ((World)world).createExplosion(null, pos.getX(), pos.getY(), pos.getZ(), 6.0F, true);
         *
         * New code:
         * // fix ClassCastException when fire interacts with Galacticraft fluids
         * Hooks.getWorld(world).createExplosion(null, pos.getX(), pos.getY(), pos.getZ(), 6.0F, true);
         */
        if(insn.getOpcode() == CHECKCAST && ((TypeInsnNode)insn).desc.equals("net/minecraft/world/World")) {
            method.instructions.insert(insn, genMethodNode("getWorld", "(Lnet/minecraft/world/IBlockAccess;)Lnet/minecraft/world/World;"));
            method.instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        if(isFluidUtil) {
            /*
             * New code:
             * // account for FluidStates
             * @SideOnly(Side.CLIENT)
             * public static boolean isInsideOfFluid(Entity entity, Fluid fluid)
             * {
             *     return Hooks.isInsideOfFluid(entity, fluid);
             * }
             */
            overrideMethod(classNode, method -> "isInsideOfFluid".equals(method.name), "isInsideOfFluid", "(Lnet/minecraft/entity/Entity;Lnet/minecraftforge/fluids/Fluid;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
            });
            return false;
        }

        else {
            classNode.methods.removeIf(method -> method.name.equals("getExtendedState"));
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
            return true;
        }
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static World getWorld(@Nonnull final IBlockAccess access) {
            // Modern java versions can have issues with ASM referring to an interface static method directly.
            return IWorldProvider.getWorld(access);
        }

        @Nullable
        public static Boolean isEntityInsideMaterial(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final Entity entity, final double yToTest, @Nonnull final Material material, final boolean testingHead) {
            return FluidCollisionHandler.isEntityInsideMaterial(access, pos, state, entity, yToTest, material == Material.WATER ? state.getMaterial() : material, testingHead);
        }

        @SideOnly(Side.CLIENT)
        public static boolean isInsideOfFluid(@Nullable final Entity entity, @Nullable final Fluid fluid) {
            return entity != null && FluidloggedUtils.getFluidFromState(ActiveRenderInfo.getBlockStateAtEntityViewpoint(ClientProxyCore.mc.world, entity, ClientProxyCore.mc.getRenderPartialTicks())) == fluid;
        }
    }
}
