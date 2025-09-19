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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.lycanitesmobs;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidMixHandler;
import net.minecraft.block.material.Material;
import net.minecraft.init.Blocks;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.Collections;

/**
 * fix lycanites fluid mixing
 * @author jbred
 *
 */
public abstract class PluginLycanitesFluidMixing implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // Fix lycanites fluid mixing
         * @ASMGenerated
         * public void updateTick(World world, BlockPos pos, IBlockState state, Random rand)
         * {
         *     FluidMixHandler.tryMixAtNeighbors(FluidState.of(state), world, pos, Hooks.getMixConditions(), null);
         *     super.updateTick(world, pos, state, rand);
         * }
         */
        addMethod(classNode, obfuscated ? "func_180650_b" : "updateTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 3);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "of", "(Lnet/minecraft/block/state/IBlockState;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", false);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "getMixConditions", "()Ljava/util/Collection;", false);
            generator.visitInsn(ACONST_NULL);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/common/fluid/handler/FluidMixHandler", "tryMixAtNeighbors", "(Lgit/jbredwards/fluidlogged_api/api/util/FluidState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Ljava/util/Collection;Ljava/util/function/BiConsumer;)Z", false);
            generator.visitInsn(POP);
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ALOAD, 4);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", obfuscated ? "func_180650_b" : "updateTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V", false);
        });

        return false;
    }

    // -------------------------------------------
    // use different mix conditions for each fluid
    // -------------------------------------------

    public static final class Acid extends PluginLycanitesFluidMixing
    {
        @SuppressWarnings("unused")
        public static final class Hooks
        {
            @Nonnull
            public static Collection<FluidMixHandler.MixCondition> getMixConditions() {
                return Collections.singleton(FluidMixHandler.forFluid((source, access, pos, sourcePos, side) -> {
                    @Nonnull final FluidState fluid = FluidloggedUtils.getFluidState(access, pos);
                    if(fluid.getMaterial() == Material.WATER) return Blocks.STONE.getStateFromMeta(3);
                    else if(fluid.getMaterial() == Material.LAVA) return Blocks.STONE.getStateFromMeta(1);
                    else return null;
                }));
            }
        }
    }

    public static final class Ooze extends PluginLycanitesFluidMixing
    {
        @SuppressWarnings("unused")
        public static final class Hooks
        {
            @Nonnull
            public static Collection<FluidMixHandler.MixCondition> getMixConditions() {
                return Collections.singleton(FluidMixHandler.forFluid((source, access, pos, sourcePos, side) -> {
                    @Nonnull final FluidState fluid = FluidloggedUtils.getFluidState(access, pos);
                    if(fluid.getMaterial() == Material.WATER) return Blocks.PACKED_ICE.getDefaultState();
                    else if(fluid.getMaterial() == Material.LAVA) return Blocks.OBSIDIAN.getDefaultState();
                    else return null;
                }));
            }
        }
    }

    public static final class Poison extends PluginLycanitesFluidMixing
    {
        @SuppressWarnings("unused")
        public static final class Hooks
        {
            @Nonnull
            public static Collection<FluidMixHandler.MixCondition> getMixConditions() {
                return Collections.singleton(FluidMixHandler.forFluid((source, access, pos, sourcePos, side) -> {
                    @Nonnull final FluidState fluid = FluidloggedUtils.getFluidState(access, pos);
                    if(fluid.getMaterial() == Material.WATER) return Blocks.STONE.getStateFromMeta(5);
                    else if(fluid.getMaterial() == Material.LAVA) return Blocks.STONE.getStateFromMeta(1);
                    else return null;
                }));
            }
        }
    }

    public static final class Veshoney extends PluginLycanitesFluidMixing
    {
        @SuppressWarnings("unused")
        public static final class Hooks
        {
            @Nonnull
            public static Collection<FluidMixHandler.MixCondition> getMixConditions() {
                return Collections.singleton(FluidMixHandler.forFluid((source, access, pos, sourcePos, side) -> {
                    @Nonnull final FluidState fluid = FluidloggedUtils.getFluidState(access, pos);
                    if(fluid.getMaterial() == Material.WATER) return Blocks.DIRT.getDefaultState();
                    else if(fluid.getMaterial() == Material.LAVA) return Blocks.COBBLESTONE.getDefaultState();
                    else return null;
                }));
            }
        }
    }
}
