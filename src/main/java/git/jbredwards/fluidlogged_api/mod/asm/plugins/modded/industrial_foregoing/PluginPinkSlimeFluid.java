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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.industrial_foregoing;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Make Industrial Foregoing's pink slime fluid work better with FluidStates.
 * @author jbred
 *
 */
public final class PluginPinkSlimeFluid implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_180645_a" : "randomTick"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * worldIn.setBlockToAir(pos);
         *
         * New code:
         * // Account for FluidStates.
         * FluidloggedUtils.setFluidToAir(worldIn, pos);
         */
        if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
            instructions.insertBefore(insn, new InsnNode(ACONST_NULL));
            instructions.insertBefore(insn, new InsnNode(ICONST_3));
            instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "setFluidToAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
        }
        /*
         * Old code:
         * pinkSlime.setPosition((double)pos.getX() + 0.5D, (double)pos.getY(), (double)pos.getZ() + 0.5D);
         *
         * New code:
         * // Summon the Pink Slime on top of the block at this position.
         * pinkSlime.setPosition((double)pos.getX() + 0.5D, Hooks.getY(pos, worldIn), (double)pos.getZ() + 0.5D);
         */
        else if(checkMethod(insn, obfuscated ? "func_177956_o" : "getY")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, genMethodNode("getY", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/World;)D"));
            instructions.remove(insn.getNext());
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static double getY(@Nonnull final BlockPos pos, @Nonnull final World world) {
            @Nullable final AxisAlignedBB bb = world.getBlockState(pos).getCollisionBoundingBox(world, pos);
            return bb != null ? bb.maxY + pos.getY() : pos.getY();
        }
    }
}
