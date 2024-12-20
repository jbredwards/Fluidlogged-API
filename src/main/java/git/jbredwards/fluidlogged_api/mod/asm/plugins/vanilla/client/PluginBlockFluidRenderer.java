/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.FluidRegistry;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * allow the vanilla fluid renderer to recognize FluidStates
 * @author jbred
 *
 */
public final class PluginBlockFluidRenderer implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_178269_a" : "getFluidHeight"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        if(insn.getOpcode() == ALOAD && ((VarInsnNode)insn).var == 3) {
            /*
             * getFluidHeight: (changes are around line 282)
             * Old code:
             * if (blockAccess.getBlockState(blockpos.up()).getMaterial() == blockMaterial)
             * {
             *     ...
             * }
             *
             * New code:
             * // Account for FluidStates and IComparableFluid
             * if (Hooks.matchMaterialIfFluid(blockAccess, blockpos.up(), blockMaterial) == blockMaterial)
             * {
             *     ...
             * }
             */
            if(checkMethod(insn.getPrevious(), obfuscated ? "func_185904_a" : "getMaterial")) {
                removeFrom(instructions, insn.getPrevious(), -1);
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 3));
                instructions.insertBefore(insn, genMethodNode("matchMaterialIfFluid", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/material/Material;)Lnet/minecraft/block/material/Material;"));
            }
            /*
             * getFluidHeight: (changes are around line 290)
             * Old code:
             * if (material != blockMaterial)
             * {
             *     ...
             * }
             *
             * New code:
             * // Account for FluidStates and IComparableFluid
             * if (Hooks.matchMaterialIfFluid(fluidState, blockMaterial) != blockMaterial)
             * {
             *     ...
             * }
             */
            else {
                instructions.remove(insn.getPrevious());
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 15));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 3));
                instructions.insertBefore(insn, genMethodNode("matchMaterialIfFluid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/material/Material;)Lnet/minecraft/block/material/Material;"));
            }
        }
        /*
         * getFluidHeight: (changes are around line 287)
         * Old code:
         * IBlockState iblockstate = blockAccess.getBlockState(blockpos);
         *
         * New code:
         * // Add FluidState local variable
         * IBlockState iblockstate = blockAccess.getBlockState(blockpos);
         * IBlockState fluidState = FluidloggedUtils.getFluidOrReal(blockAccess, blockpos, iblockstate);
         */
        else if(insn.getOpcode() == ASTORE && ((VarInsnNode)insn).var == 8) {
            instructions.insert(insn, new VarInsnNode(ASTORE, 15));
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 8));
            instructions.insert(insn, new VarInsnNode(ALOAD, 7));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
        }
        /*
         * getFluidHeight: (changes around line 300)
         * Old code:
         * int k = ((Integer)iblockstate.getValue(BlockLiquid.LEVEL)).intValue();
         *
         * New code:
         * // Use FluidState for level
         * int k = ((Integer)fluidState.getValue(BlockLiquid.LEVEL)).intValue();
         */
        else if(insn.getOpcode() == ALOAD && ((VarInsnNode)insn).var == 8 && checkField(insn.getNext(), obfuscated ? "field_176367_b" : "LEVEL")) {
            ((VarInsnNode)insn).var = 15;
            return true;
        }

        return false;
    }

    @Override
    public boolean addLocalVariables(@Nonnull final MethodNode method, @Nonnull final LabelNode start, @Nonnull final LabelNode end, final int index) {
        method.localVariables.add(new LocalVariableNode("fluidState", "Lnet/minecraft/block/state/IBlockState;", null, start, end, 15));
        return true;
    }

    @Override
    public boolean recalcFrames(final boolean obfuscated) { return true; }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static Material matchMaterialIfFluid(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final Material toMatch) {
            return matchMaterialIfFluid(FluidloggedUtils.getFluidOrReal(access, pos), toMatch);
        }

        @Nonnull
        public static Material matchMaterialIfFluid(@Nonnull final IBlockState fluidState, @Nonnull final Material toMatch) {
            return FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidFromState(fluidState), toMatch == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA) ? toMatch : Material.AIR;
        }
    }
}
