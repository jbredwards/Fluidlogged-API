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
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * -fixes underwater block selection, for real though, it's hard-coded to not render underwater, but if I remove that, works fine underwater, WHY?! lol
 * -lava FluidStates now emit smoke while raining
 * -fixes FluidState fog color
 * -correct rain particle spawn height
 * @author jbred
 *
 */
public final class PluginEntityRenderer implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, obfuscated ? "func_175068_a" : "renderWorldPass", "(IFJ)V")) return 1;
        else if(checkMethod(method, obfuscated ? "func_78484_h" : "addRainParticles", "()V")) return 2;
        //updateFogColor, line 1857
        else if(checkMethod(method, obfuscated ? "func_78466_h" : "updateFogColor", "(F)V"))
            return 3;

        else return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * renderWorldPass: (changes are around line 1409)
         * Old code:
         * if (flag && this.mc.objectMouseOver != null && !entity.isInsideOfMaterial(Material.WATER))
         * {
         *     ...
         * }
         *
         * New code:
         * //remove bad check that exists for no reason
         * if (flag && this.mc.objectMouseOver != null && !false)
         * {
         *     ...
         * }
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_70055_a" : "isInsideOfMaterial", "(Lnet/minecraft/block/material/Material;)Z")) {
            instructions.insert(insn, new InsnNode(ICONST_0));
            removeFrom(instructions, insn, -2);
            return true;
        }
        //addRainParticles
        else if(index == 2) {
            /*
             * addRainParticles: (changes are around line 1559)
             * Old code:
             * AxisAlignedBB axisalignedbb = iblockstate.getBoundingBox(world, blockpos2);
             *
             * New code:
             * //replace old height with better one
             * AxisAlignedBB axisalignedbb = Hooks.fixRainCollision(iblockstate, world, blockpos2);
             */
            if(checkMethod(insn, obfuscated ? "func_185900_c" : "getBoundingBox")) {
                instructions.insert(insn, genMethodNode("fixRainCollision", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/util/math/AxisAlignedBB;"));
                instructions.remove(insn);
            }
            /*
             * addRainParticles: (changes are around line 1561)
             * Old code:
             * if (iblockstate.getMaterial() != Material.LAVA && iblockstate.getBlock() != Blocks.MAGMA)
             * {
             *     ...
             * }
             *
             * New code:
             * //only do lava smoke particles if they make sense
             * if (Hooks.addRainParticles(iblockstate, blockpos2) && iblockstate.getBlock() != Blocks.MAGMA)
             * {
             *     ...
             * }
             */
            else if(checkField(insn, obfuscated ? "field_151587_i" : "LAVA", "Lnet/minecraft/block/material/Material;")) {
                final InsnList list = new InsnList();
                //adds new code
                list.add(new VarInsnNode(ALOAD, 17));
                list.add(genMethodNode("addRainParticles", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/BlockPos;)Z"));
                ((JumpInsnNode)insn.getNext()).setOpcode(IFEQ);
                instructions.insert(insn, list);
                //removes old
                removeFrom(instructions, insn, -1);
            }
            /*
             * addRainParticles: (changes are around lines 1570 & 1574)
             * Old code:
             * d1 = (double)((float)blockpos2.getY() + 0.1F) + axisalignedbb.maxY - 1.0D;
             * this.mc.world.spawnParticle(EnumParticleTypes.WATER_DROP, (double)blockpos2.getX() + d3, (double)((float)blockpos2.getY() + 0.1F) + axisalignedbb.maxY, (double)blockpos2.getZ() + d4, 0.0D, 0.0D, 0.0D, new int[0]);
             *
             * New code:
             * //remove unnecessary offset
             * d1 = (double)((float)blockpos2.getY()) + axisalignedbb.maxY - 1.0D;
             * this.mc.world.spawnParticle(EnumParticleTypes.WATER_DROP, (double)blockpos2.getX() + d3, (double)((float)blockpos2.getY()) + axisalignedbb.maxY, (double)blockpos2.getZ() + d4, 0.0D, 0.0D, 0.0D, new int[0]);
             */
            else if(checkField(insn, obfuscated ? "field_72337_e" : "maxY"))
                removeFrom(instructions, getPrevious(insn, 3), -1);
            /*
             * addRainParticles: (changes are around line 1579)
             * Old code:
             * this.mc.world.spawnParticle(EnumParticleTypes.SMOKE_NORMAL, (double)blockpos1.getX() + d3, (double)((float)blockpos1.getY() + 0.1F) - axisalignedbb.minY, (double)blockpos1.getZ() + d4, 0.0D, 0.0D, 0.0D, new int[0]);
             *
             * New code:
             * //lava height works differently than water for some reason, lets change that
             * this.mc.world.spawnParticle(EnumParticleTypes.SMOKE_NORMAL, (double)blockpos1.getX() + d3, (double)((float)blockpos1.getY() - 1F) + axisalignedbb.maxY, (double)blockpos1.getZ() + d4, 0.0D, 0.0D, 0.0D, new int[0]);
             */
            else if(checkField(insn, obfuscated ? "field_72338_b" : "minY")) {
                ((FieldInsnNode)insn).name = obfuscated ? "field_72337_e" : "maxY";
                instructions.remove(insn.getNext());
                instructions.insert(insn, new InsnNode(DADD));
                //vanilla uses the wrong blockpos here, fix that
                instructions.insert(getPrevious(insn, 5), new InsnNode(FCONST_1));
                instructions.insert(getPrevious(insn, 5), new InsnNode(FSUB));
                //remove unnecessary offset
                removeFrom(instructions, getPrevious(insn, 3), -1);
                return true;
            }
        }
        /*
         * updateFogColor: (changes are around line 1857)
         * Old code:
         * IBlockState viewportState = this.mc.world.getBlockState(viewportPos);
         *
         * New code:
         * // this mod changes how the view state works, so there's no need to define a different one here
         * IBlockState viewportState = iblockstate;
         */
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new VarInsnNode(ALOAD, 12));
            removeFrom(instructions, insn, -4);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean addRainParticles(@Nonnull IBlockState here, @Nonnull BlockPos pos) {
            final WorldClient world = Minecraft.getMinecraft().world;
            final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos, here);

            return fluidState.isEmpty() || fluidState.getMaterial() != Material.LAVA
                    || !FluidloggedUtils.canFluidConnect(world, pos, here, EnumFacing.UP);
        }

        @Nonnull
        public static AxisAlignedBB fixRainCollision(@Nonnull IBlockState here, @Nonnull World world, @Nonnull BlockPos pos) {
            final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos, here);
            //skip fluid check if none are present, or if it's a bad fluid
            if(fluidState.isEmpty() || !fluidState.isValid()) return here.getBoundingBox(world, pos);
            final double maxY = here.getMaterial().blocksMovement() ? here.getBoundingBox(world, pos).maxY : 0;
            final double fluidHeight = Math.max(FluidloggedUtils.isFluid(here) ? 0 : maxY, FluidCollisionHandler.getFilledPercentage(fluidState, world, pos));
            return new AxisAlignedBB(0, 0, 0, 1, fluidHeight, 1);
        }
    }
}
