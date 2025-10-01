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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.astral_sorcery;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.Fluid;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fixes weird mixing interactions
 * @author jbred
 *
 */
public final class PluginAstralSorcery implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("interactWithAdjacent"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * interactWithAdjacent:
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * //cache state here & isReplaceable for later use
         * {
         *     IBlockState here = world.getBlockState(pos);
         *     boolean isHereReplaceable = here.getBlock().isReplaceable(world, pos);
         *     ...
         * }
         */
        if(insn.getPrevious() == instructions.getFirst()) {
            final InsnList list = new InsnList();
            //state
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            list.add(new VarInsnNode(ASTORE, 11));
            //isReplaceable
            list.add(new VarInsnNode(ALOAD, 11));
            list.add(new MethodInsnNode(INVOKEINTERFACE, "net/minecraft/block/state/IBlockState", obfuscated ? "func_177230_c" : "getBlock", "()Lnet/minecraft/block/Block;", true));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/block/Block", obfuscated ? "func_176200_f" : "isReplaceable", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", false));
            list.add(new VarInsnNode(ISTORE, 12));
            instructions.insertBefore(insn, list);
        }
        /*
         * interactWithAdjacent:
         * Old code:
         * if (side != EnumFacing.DOWN)
         * {
         *     ...
         * }
         *
         * New code:
         * //check if the fluid here can flow into the given side
         * if (Hooks.canFlowInto(side, this.definedFluid, world, pos, here, isHereReplaceable))
         * {
         *     ...
         * }
         */
        else if(insn.getOpcode() == IF_ACMPEQ) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 11));
            list.add(new VarInsnNode(ILOAD, 12));
            list.add(genMethodNode("canFlowInto", "(Lnet/minecraft/util/EnumFacing;Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Z)Z"));
            ((JumpInsnNode)insn).setOpcode(IFEQ);
            instructions.remove(insn.getPrevious());
            instructions.insertBefore(insn, list);
        }
        /*
         * interactWithAdjacent:
         * Old code:
         * IBlockState offset = world.getBlockState(pos.offset(side));
         *
         * New code:
         * //use fluid or real
         * IBlockState offset = FluidloggedUtils.getFluidOrReal(world, pos.offset(side));
         */
        else if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
        }
        /*
         * interactWithAdjacent:
         * Old code:
         * if (offset.getMaterial().isLiquid() && !(offset.getBlock() instanceof FluidBlockLiquidStarlight) && (offset.getBlock() instanceof BlockFluidBase || offset.getBlock() instanceof BlockLiquid))
         * {
         *     ...
         * }
         *
         * New code:
         * //previous check takes care of everything
         * if (true)
         * {
         *     ...
         * }
         */
        else if(insn.getOpcode() == INSTANCEOF && ((TypeInsnNode)insn).desc.equals("net/minecraft/block/BlockLiquid")) {
            instructions.insert(insn, new InsnNode(ICONST_1));
            removeFrom(instructions, insn, -14);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        overrideMethod(classNode, method -> method.name.equals("isEntityInsideMaterial"),
            "isEntityInsideMaterial", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(DLOAD, 5);
                generator.visitVarInsn(ALOAD, 7);
                generator.visitVarInsn(ILOAD, 8);
            }
        );

        return true;
    }

    @Override
    public boolean addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) {
        method.localVariables.add(new LocalVariableNode("here", "Lnet/minecraft/block/state/IBlockState;", null, start, end, 11));
        method.localVariables.add(new LocalVariableNode("isHereReplaceable", "Z", null, start, end, 12));
        return true;
    }

    @Override
    public boolean recalcFrames(boolean obfuscated) { return true; }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canFlowInto(@Nonnull EnumFacing side, @Nonnull Fluid fluid, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, boolean isHereReplaceable) {
            if(!isHereReplaceable || side == EnumFacing.DOWN || FluidloggedAPIConfig.fixBadFluidMixing && !FluidloggedUtils.canFluidFlow(world, pos, here, side)) return false;

            final BlockPos neighborPos = pos.offset(side);
            final Chunk chunk = world.getChunk(neighborPos);
            final IBlockState neighbor = chunk.getBlockState(neighborPos);

            if(FluidloggedAPIConfig.fixBadFluidMixing && !FluidloggedUtils.canFluidFlow(world, neighborPos, neighbor, side.getOpposite())) return false;
            final Fluid neighborFluid = FluidloggedUtils.getFluidState(chunk, neighborPos, neighbor).getFluid();
            return neighborFluid != null && !FluidloggedUtils.isCompatibleFluid(fluid, neighborFluid);
        }

        @Nullable
        public static Boolean isEntityInsideMaterial(@Nonnull final IBlockAccess access, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final Entity entity, final double yToTest, @Nonnull final Material material, final boolean testingHead) {
            return FluidCollisionHandler.isEntityInsideMaterial(access, pos, state, entity, yToTest, material == Material.WATER ? state.getMaterial() : material, testingHead);
        }
    }
}
