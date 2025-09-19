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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.flopper;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginFluidUtil;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.FluidStack;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * make floppers account for FluidStates
 * @author jbred
 *
 */
public final class PluginFlopper implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if("pushFluidsToWorld".equals(method.name)) return 1;
        else return "pullFluidsFromWorld".equals(method.name) ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * pushFluidsToWorld:
         * Old code:
         * if (this.world.isAirBlock(targetPos)
         *         || ...
         *
         * New code:
         * // Also allow fluids to be placed in fluidloggable blocks
         * if (Hooks.isAirOrFluidloggable(this.world, targetPos, destBlockState, this.tank.getFluid())
         *         || ...
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "org/cyclops/flopper/tileentity/TileFlopper", "tank", "Lorg/cyclops/cyclopscore/fluid/Tank;"));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "org/cyclops/cyclopscore/fluid/Tank", "getFluid", "()Lnet/minecraftforge/fluids/FluidStack;", false));
            instructions.insertBefore(insn, genMethodNode("isAirOrFluidloggable", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraftforge/fluids/FluidStack;)Z"));
            instructions.remove(insn);
            return true;
        }
        /*
         * pullFluidsFromWorld:
         * Old code:
         * IFluidHandler fluidHandler = wrapFluidBlock(destBlockState.getBlock(), world, targetPos);
         *
         * New code:
         * // Account for FluidStates
         * IFluidHandler fluidHandler = wrapFluidBlock(FluidloggedUtils.getFluidOrReal(this.world, targetPos, destBlockState).getBlock(), world, targetPos);
         */
        else if(index == 2 && insn.getOpcode() == ALOAD && ((VarInsnNode)insn).var == 2) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/tileentity/TileEntity", obfuscated ? "field_145850_b" : "world", "Lnet/minecraft/world/World;"));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isAirOrFluidloggable(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nullable final FluidStack stack) {
            return state.getBlock().isAir(state, world, pos) || stack != null && PluginFluidUtil.Hooks.isFluidloggable(world, pos, stack, state);
        }
    }
}
