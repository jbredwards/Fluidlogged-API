/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import micdoodle8.mods.galacticraft.core.proxy.ClientProxyCore;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.objectweb.asm.tree.ClassNode;

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
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
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
