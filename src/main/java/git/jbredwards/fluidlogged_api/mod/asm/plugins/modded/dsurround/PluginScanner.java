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
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Random;

/**
 * Also scan FluidStates.
 * @author jbred
 *
 */
public final class PluginScanner implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_73660_a" : "update") || method.name.equals("updateScan")) return 1;
        else return method.name.equals("interestingBlock") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final ClassNode classNode, @Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * update & updateScan:
         * Old code:
         * IBlockState state = provider.getBlockState(pos);
         *
         * New code:
         * // Also scan FluidStates.
         * IBlockState state = Hooks.getStateAndScanFluid(provider, pos, this, this.random);
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "org/orecruncher/dsurround/lib/scanner/Scanner", "random", "Ljava/util/Random;"));
            instructions.insertBefore(insn, genMethodNode("getStateAndScanFluid", withAccessorClass("(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;L%s;Ljava/util/Random;)Lnet/minecraft/block/state/IBlockState;")));
            instructions.remove(insn);
            return true;
        }

        // interestingBlock: Improve method visibility.
        else if(index == 2) {
            classNode.interfaces.add(getAccessorClass());
            method.access &= ~ACC_PROTECTED;
            method.access |= ACC_PUBLIC;
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public interface Accessor
    {
        void blockScan(@Nonnull final IBlockState state, @Nonnull final BlockPos pos, @Nonnull final Random random);
        boolean interestingBlock(@Nonnull final IBlockState state);
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static IBlockState getStateAndScanFluid(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final Accessor scanner, @Nonnull final Random random) {
            @Nonnull final ICubeData cube = ICubeData.get(access, pos);
            @Nonnull final IBlockState state = cube.getBlockState(pos);

            if(!FluidloggedUtils.isFluid(state) && state.getMaterial() != Material.AIR) {
                @Nonnull final FluidState fluidState = cube.getFluidState(pos);
                if(!fluidState.isEmpty() && scanner.interestingBlock(fluidState.getState())) scanner.blockScan(fluidState.getState(), pos, random);
            }

            return state;
        }
    }
}
