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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * these particles don't instantly disappear while inside water FluidStates
 * @author jbred
 *
 */
public final class PluginWaterParticles implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_189213_a" : "onUpdate"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * onUpdate: (changes are around line 38)
         * Old code:
         * if (this.world.getBlockState(new BlockPos(this.posX, this.posY, this.posZ)).getMaterial() != Material.WATER)
         * {
         *     ...
         * }
         *
         * New code:
         * //this particle doesn't disappear while within a water FluidState
         * if (Hooks.getFluidMaterial(this.world, new BlockPos(this.posX, this.posY, this.posZ), this.posY).getMaterial() != Material.WATER)
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/client/particle/Particle", obfuscated ? "field_187127_g" : "posY", "D"));
            instructions.insertBefore(insn, genMethodNode( "getFluidMaterial", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;D)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static IBlockState getFluidMaterial(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, final double posY) {
            return (FluidCollisionHandler.isYWithinMaterialEstimate(access, pos, posY, posY, Material.WATER) ? Blocks.WATER : Blocks.AIR).getDefaultState();
        }
    }
}
