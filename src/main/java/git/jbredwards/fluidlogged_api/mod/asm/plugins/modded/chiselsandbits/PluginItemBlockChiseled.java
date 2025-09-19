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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.chiselsandbits;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * allow chisel blocks to be placed in replaceable blocks
 * @author jbred
 *
 */
public final class PluginItemBlockChiseled implements IASMPlugin
{
    private final int posVar;
    public PluginItemBlockChiseled(final int posVarIn) { posVar = posVarIn; }

    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("tryPlaceBlockAt") || method.name.equals("doAction"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * if (world.getBlockState(bp).getBlock().isReplaceable(world, bp))
         * {
         *     ...
         * }
         *
         * New code:
         * // Don't set the block here to air before placing the chiseled block
         * if (false)
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, obfuscated ? "func_176200_f" : "isReplaceable") && insn.getPrevious().getOpcode() == ALOAD && ((VarInsnNode)insn.getPrevious()).var == posVar) {
            instructions.insert(insn, new InsnNode(ICONST_0));
            removeFrom(instructions, insn, -6);
        }
        /*
         * Old code:
         * if (world.isAirBlock(bp))
         * {
         *     ...
         * }
         *
         * New code:
         * // Move the check for replaceable blocks
         * if (Hooks.isAirOrReplaceable(world, bp, state))
         * {
         *     ...
         * }
         */
        else if(checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock") && insn.getPrevious().getOpcode() == ALOAD && ((VarInsnNode)insn.getPrevious()).var == 16) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 17));
            instructions.insertBefore(insn, genMethodNode("isAirOrReplaceable", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isAirOrReplaceable(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final IBlockState state) {
            return state.getBlock().isAir(state, access, pos) || state.getBlock().isReplaceable(access, pos);
        }
    }
}
