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
    private int pos, f7, f8, f9, f10, d1; // indexes

    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_178270_a" : "renderFluid")) {
            f7 = findLocal(method, "f7", "F").index;
            f8 = findLocal(method, "f8", "F").index;
            f9 = findLocal(method, "f9", "F").index;
            f10 = findLocal(method, "f10", "F").index;
            d1 = findLocal(method, "d1", "D").index;
            return 1;
        }

        else return method.name.equals(obfuscated ? "func_178269_a" : "getFluidHeight") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        // renderFluid
        if(index == 1) {
            if(checkMethod(insn, obfuscated ? "func_181662_b" : "pos") && ++pos == 12) return true;
            /*
             * renderFluid: (changes are around lines 127-130 & 134-137)
             * Old code:
             * bufferBuilderIn.pos(d0 + 0.0D, d1 + (double)f7, d2 + 0.0D).color(f24, f25, f26, 1.0F).tex((double)f13, (double)f17).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 0.0D, d1 + (double)f8, d2 + 1.0D).color(f24, f25, f26, 1.0F).tex((double)f14, (double)f18).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + (double)f9, d2 + 1.0D).color(f24, f25, f26, 1.0F).tex((double)f15, (double)f19).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + (double)f10, d2 + 0.0D).color(f24, f25, f26, 1.0F).tex((double)f16, (double)f20).lightmap(l2, i3).endVertex();
             * ...
             * bufferBuilderIn.pos(d0 + 0.0D, d1 + (double)f7, d2 + 0.0D).color(f24, f25, f26, 1.0F).tex((double)f13, (double)f17).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + (double)f10, d2 + 0.0D).color(f24, f25, f26, 1.0F).tex((double)f16, (double)f20).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + (double)f9, d2 + 1.0D).color(f24, f25, f26, 1.0F).tex((double)f15, (double)f19).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 0.0D, d1 + (double)f8, d2 + 1.0D).color(f24, f25, f26, 1.0F).tex((double)f14, (double)f18).lightmap(l2, i3).endVertex();
             *
             * New code:
             * // prevent z-fighting on top vertex, by forcing it to be less than 1
             * bufferBuilderIn.pos(d0 + 0.0D, d1 + (double)Math.min(f7, 0.998f), d2 + 0.0D).color(f24, f25, f26, 1.0F).tex((double)f13, (double)f17).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 0.0D, d1 + (double)Math.min(f8, 0.998f), d2 + 1.0D).color(f24, f25, f26, 1.0F).tex((double)f14, (double)f18).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + (double)Math.min(f9, 0.998f), d2 + 1.0D).color(f24, f25, f26, 1.0F).tex((double)f15, (double)f19).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + (double)Math.min(f10, 0.998f), d2 + 0.0D).color(f24, f25, f26, 1.0F).tex((double)f16, (double)f20).lightmap(l2, i3).endVertex();
             * ...
             * bufferBuilderIn.pos(d0 + 0.0D, d1 + (double)Math.min(f7, 0.998f), d2 + 0.0D).color(f24, f25, f26, 1.0F).tex((double)f13, (double)f17).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + (double)Math.min(f10, 0.998f), d2 + 0.0D).color(f24, f25, f26, 1.0F).tex((double)f16, (double)f20).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + (double)Math.min(f9, 0.998f), d2 + 1.0D).color(f24, f25, f26, 1.0F).tex((double)f15, (double)f19).lightmap(l2, i3).endVertex();
             * bufferBuilderIn.pos(d0 + 0.0D, d1 + (double)Math.min(f8, 0.998f), d2 + 1.0D).color(f24, f25, f26, 1.0F).tex((double)f14, (double)f18).lightmap(l2, i3).endVertex();
             */
            else if(insn.getOpcode() == FLOAD && (((VarInsnNode)insn).var == f7 || ((VarInsnNode)insn).var == f8 || ((VarInsnNode)insn).var == f9 || ((VarInsnNode)insn).var == f10)) {
                instructions.insert(insn, genMethodNode("java/lang/Math", "min", "(FF)F"));
                instructions.insert(insn, new LdcInsnNode(0.998f));
            }
            /*
             * renderFluid: (changes are around lines 150-153)
             * Old code:
             * bufferBuilderIn.pos(d0, d1, d2 + 1.0D).color(0.5F, 0.5F, 0.5F, 1.0F).tex((double)f35, (double)f38).lightmap(i2, j2).endVertex();
             * bufferBuilderIn.pos(d0, d1, d2).color(0.5F, 0.5F, 0.5F, 1.0F).tex((double)f35, (double)f37).lightmap(i2, j2).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1, d2).color(0.5F, 0.5F, 0.5F, 1.0F).tex((double)f36, (double)f37).lightmap(i2, j2).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1, d2 + 1.0D).color(0.5F, 0.5F, 0.5F, 1.0F).tex((double)f36, (double)f38).lightmap(i2, j2).endVertex();
             *
             * New code:
             * // prevent z-fighting on bottom vertex, by forcing it to be greater than 0
             * bufferBuilderIn.pos(d0, d1 + 0.002D, d2 + 1.0D).color(0.5F, 0.5F, 0.5F, 1.0F).tex((double)f35, (double)f38).lightmap(i2, j2).endVertex();
             * bufferBuilderIn.pos(d0, d1 + 0.002D, d2).color(0.5F, 0.5F, 0.5F, 1.0F).tex((double)f35, (double)f37).lightmap(i2, j2).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + 0.002D, d2).color(0.5F, 0.5F, 0.5F, 1.0F).tex((double)f36, (double)f37).lightmap(i2, j2).endVertex();
             * bufferBuilderIn.pos(d0 + 1.0D, d1 + 0.002D, d2 + 1.0D).color(0.5F, 0.5F, 0.5F, 1.0F).tex((double)f36, (double)f38).lightmap(i2, j2).endVertex();
             */
            else if(pos >= 8 && insn.getOpcode() == DLOAD && ((VarInsnNode)insn).var == d1) {
                instructions.insert(insn, new InsnNode(DADD));
                instructions.insert(insn, new LdcInsnNode(0.002));
            }
        }
        // getFluidHeight
        else if(index == 2) {
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
