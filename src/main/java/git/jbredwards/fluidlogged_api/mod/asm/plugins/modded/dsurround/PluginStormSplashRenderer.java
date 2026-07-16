/*
 * Copyright (C) <2026 to Present> <jbredwards>
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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dsurround;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.ICubeData;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * Account for FluidStates and fix particle y position
 * @author jbred
 *
 */
public final class PluginStormSplashRenderer implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(method.name.equals("playSplashSound")) return 1;
        else return method.name.equals("addRainParticles") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * playSplashSound & addRainParticles:
         * Old code:
         * IBlockState state = ClientChunkCache.instance().getBlockState(blockPos);
         *
         * New code:
         * // Use FluidState, unless fluidlogged block collides before it.
         * IBlockState state = Hooks.getExposedFluid(ClientChunkCache.instance(), blockPos);
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("getExposedFluid", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return index == 1;
        }
        /*
         * addRainParticles:
         * Old code:
         * double posY = (double)((float)precipHeight.getY() + 0.1F) - state.getBoundingBox(world, blockPos).minY;
         *
         * New code:
         * // Fix particle y position.
         * double posY = (double)((float)precipHeight.getY() + 0.1F) - Hooks.fixParticleY(state, world, blockPos);
         */
        else if(checkMethod(insn, obfuscated ? "func_185900_c" : "getBoundingBox")) {
            instructions.insertBefore(insn, genMethodNode("fixParticleY", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)D"));
            removeFrom(instructions, insn, 1);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static double fixParticleY(@Nonnull final IBlockState state, @Nonnull final IBlockAccess access, @Nonnull final BlockPos pos) {
            @Nonnull final FluidState fluidState = FluidState.of(state);
            return 1 - (fluidState.isEmpty() ? state.getBoundingBox(access, pos).maxY : (fluidState.getHeight() + 0.1f));
        }

        @Nonnull
        public static IBlockState getExposedFluid(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos) {
            @Nonnull final ICubeData cube = ICubeData.get(access, pos);
            @Nonnull final IBlockState state = cube.getBlockState(pos);

            if(FluidloggedUtils.isFluid(state)) return state;
            final boolean solid = !state.getBlock().isPassable(access, pos);
            if(solid && !FluidloggedUtils.canFluidConnect(access, pos, state, EnumFacing.UP)) return state;

            @Nonnull final FluidState fluidState = cube.getFluidState(pos);
            return !solid || fluidState.getHeight() > state.getBoundingBox(access, pos).maxY ? fluidState.getState() : state;
        }
    }
}
