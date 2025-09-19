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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.betweenlands;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * keep FluidExtendedBlockState at the time of rendering
 * @author jbred
 *
 */
public final class PluginBetweenlandsStates implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("getExtendedState"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * getExtendedState:
         * Old code:
         * IExtendedBlockState state = (IExtendedBlockState)super.getExtendedState(oldState, worldIn, pos);
         *
         * New code:
         * // call super.getExtendedState at the end of this method, so this mod's FluidExtendedBlockState can be kept at the time of rendering
         * IExtendedBlockState state = (IExtendedBlockState)oldState;
         */
        if(checkMethod(insn, "getExtendedState")) {
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            removeFrom(instructions, insn, -4);
        }
        /*
         * getExtendedState:
         * Old code:
         * return ...
         *
         * New code:
         * // restore super.getExtendedState call
         * state = (IExtendedBlockState)...
         * return super.getExtendedState(state, worldIn, pos)
         */
        else if(insn.getOpcode() == ARETURN) {
            instructions.insertBefore(insn, new TypeInsnNode(CHECKCAST, "net/minecraftforge/common/property/IExtendedBlockState"));
            instructions.insertBefore(insn, new VarInsnNode(ASTORE, 4));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 4));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 3));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", "getExtendedState", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            return true;
        }

        return false;
    }
}
