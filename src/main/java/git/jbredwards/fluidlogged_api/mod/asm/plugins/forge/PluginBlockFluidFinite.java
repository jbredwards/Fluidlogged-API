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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import it.unimi.dsi.fastutil.objects.AbstractObject2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Random;

/**
 * modded finite fluids work properly with the mod
 * @author jbred
 *
 */
public final class PluginBlockFluidFinite implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * Implement IFluidloggableFluid, allows modded fluids that extend this class to be fluidloggable by default
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/fluid/IFluidloggableFluid");
        /*
         * getQuantaValue:
         * New code:
         * //allow this method to be able to get the quanta value from FluidStates
         * @Override
         * public int getQuantaValue(IBlockAccess world, BlockPos pos)
         * {
         *     return PluginBlockFluidClassic.Hooks.getQuantaValue(this, world, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getQuantaValue"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginBlockFluidClassic$Hooks", "getQuantaValue", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", false);
        });
        /*
         * updateTick
         * New code:
         * //determines how this fluid behaves based on FluidStates
         * @Override
         * public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand)
         * {
         *     Hooks.fluidUpdateTick(world, pos, state, rand);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180650_b" : "updateTick"),
            "fluidUpdateTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );
        /*
         * place:
         * New code:
         * //allow the place method to fluidlog blocks
         * @Override
         * public int place(World world, BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace)
         * {
         *     return Hooks.place(this, world, pos, fluidStack, doPlace);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("place"),
            "place", "(Lgit/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginBlockFluidBase$Accessor;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;Z)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
            }
        );
        /*
         * drain:
         * New code:
         * //allow the drain method to drain fluidlogged blocks
         * @Override
         * @Nullable
         * public FluidStack drain(World world, BlockPos pos, boolean doDrain)
         * {
         *     return Hooks.drain(world, pos, doDrain);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("drain"),
            "drain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Z)Lnet/minecraftforge/fluids/FluidStack;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void fluidUpdateTick(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final Random rand) {
            FluidFlowHandler.updateFinite(world, pos, FluidState.of(state), rand);
        }

        @Nonnull
        public static FluidStack drain(@Nonnull final World world, @Nonnull final BlockPos pos, final boolean doDrain) {
            @Nonnull final FluidCache cache = new FluidCache(world, pos, 0, 0);
            @Nonnull final FluidStack stack = FluidloggedUtils.getFluidState(cache, pos).createFluidStack();

            if(doDrain) FluidloggedUtils.setFluidToAir(world, pos, cache.getBlockState(pos), Constants.BlockFlags.DEFAULT);
            return stack;
        }

        public static int place(@Nonnull final PluginBlockFluidBase.Accessor block, @Nonnull final World world, @Nonnull final BlockPos pos, @Nullable final FluidStack resource, final boolean doPlace) {
            if(resource == null || resource.amount <= 0) return 0;

            @Nonnull final FluidCache cache = new FluidCache(world, pos, 1, 1);
            @Nonnull final Object2IntMap.Entry<FluidState> placeInfo = getStateForStack(block, cache, pos, resource, cache.getBlockState(pos));

            if(placeInfo.getKey().isEmpty()) return 0;
            else if(doPlace && !world.isRemote) {
                // vaporize
                if(world.provider.doesWaterVaporize() && resource.getFluid().doesVaporize(resource)) {
                    FluidloggedUtils.playVaporizeEffects(world, pos, resource);
                }
                // place FluidState
                else if(FluidloggedUtils.isStateFluidloggable(cache.getBlockState(pos), cache, pos, placeInfo.getKey())) {
                    FluidloggedUtils.setFluidState(world, pos, cache.getBlockState(pos), placeInfo.getKey(), false, Constants.BlockFlags.DEFAULT_AND_RERENDER);
                }
                // place BlockState
                else {
                    FluidUtil.destroyBlockOnFluidPlacement(world, pos);
                    world.setBlockState(pos, placeInfo.getKey().getState(), Constants.BlockFlags.DEFAULT_AND_RERENDER);
                }
            }

            return placeInfo.getIntValue();
        }

        // helper
        @Nonnull
        public static Object2IntMap.Entry<FluidState> getStateForStack(@Nonnull final PluginBlockFluidBase.Accessor block, @Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final FluidStack resource, @Nonnull final IBlockState here) {
            // ========================================================================================================
            // for finite fluid blocks, calculate the new quantity based on the resource amount and the existing amount
            // ========================================================================================================
            if(resource.amount <= 0) return new AbstractObject2IntMap.BasicEntry<>(FluidState.EMPTY, 0);
            final float quantaAmount = Fluid.BUCKET_VOLUME / block.getQuantaPerBlockFloat_Public();
            final int quantaPerBlock = block.getQuantaPerBlock_Public();
            // If the stack contains more available fluid than the full source block,
            // set a source block
            int closest = Fluid.BUCKET_VOLUME;
            int quanta = quantaPerBlock;
            if(resource.amount < closest) {
                // Figure out maximum level to match stack amount
                closest = MathHelper.floor(quantaAmount * MathHelper.floor(resource.amount / quantaAmount));
                quanta = MathHelper.floor(closest / quantaAmount);
            }

            @Nonnull final FluidState existing = FluidloggedUtils.getFluidState(access, pos, here);
            if(FluidloggedUtils.isCompatibleFluid(existing.getFluid(), resource.getFluid())) {
                final int existingQuanta = existing.getQuantaValue();
                final int missingQuanta = quantaPerBlock - existingQuanta;
                closest = Math.min(closest, MathHelper.floor(missingQuanta * quantaAmount));
                quanta = Math.min(quanta + existingQuanta, quantaPerBlock);
            }

            // If too little (or too much, technically impossible) fluid is to be placed, abort
            if(quanta < 1 || quanta > 16) return new AbstractObject2IntMap.BasicEntry<>(FluidState.EMPTY, 0);
            else return new AbstractObject2IntMap.BasicEntry<>(FluidState.of((Block)block).withLevel(quanta - 1), closest);
        }
    }
}
