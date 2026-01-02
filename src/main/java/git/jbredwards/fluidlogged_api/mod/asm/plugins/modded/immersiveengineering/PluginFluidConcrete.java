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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.immersiveengineering;

import blusunrize.immersiveengineering.common.IEContent;
import blusunrize.immersiveengineering.common.blocks.stone.BlockTypes_StoneDecoration;
import blusunrize.immersiveengineering.common.blocks.stone.BlockTypes_StoneDevices;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.apache.commons.lang3.ArrayUtils;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix issue#275
 * @author jbred
 *
 */
public final class PluginFluidConcrete implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_180650_b" : "updateTick"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * ...
         *
         * New code:
         * // Don't update concrete fluid if fluidlogged.
         * if(Accessor.isStateFluidlogged(world, pos, state, this.enableSourceFall)) return;
         * ...
         */

        @Nonnull final InsnList list = new InsnList();
        list.add(new VarInsnNode(ALOAD, 0));
        list.add(new VarInsnNode(ALOAD, 1));
        list.add(new VarInsnNode(ALOAD, 2));
        list.add(genMethodNode(getAccessorClass(), "isStateFluidlogged", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));

        @Nonnull final LabelNode label = new LabelNode();
        list.add(new JumpInsnNode(IFEQ, label));
        list.add(new InsnNode(RETURN));
        list.add(label);
        list.add(new FrameNode(F_SAME, 0, null, 0, null));

        instructions.insert(insn.getNext(), list);
        return true;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.interfaces.add(getAccessorClass());
        return true;
    }

    @SuppressWarnings("unused")
    public interface Accessor extends IFluidloggableFluid
    {
        @Nonnull
        IBlockState[] CONCRETE = {
                IEContent.blockStoneDevice.getStateFromMeta(BlockTypes_StoneDevices.CONCRETE_SHEET.getMeta()),
                IEContent.blockStoneDevice.getStateFromMeta(BlockTypes_StoneDevices.CONCRETE_QUARTER.getMeta()),
                IEContent.blockStoneDecorationSlabs.getStateFromMeta(BlockTypes_StoneDecoration.CONCRETE.getMeta()),
                IEContent.blockStoneDevice.getStateFromMeta(BlockTypes_StoneDevices.CONCRETE_THREEQUARTER.getMeta()),
                IEContent.blockStoneDecoration.getStateFromMeta(BlockTypes_StoneDecoration.CONCRETE.getMeta())
        };

        @Override
        default boolean isStateFluidloggable(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
            return !ArrayUtils.contains(CONCRETE, state) && IFluidloggableFluid.super.isStateFluidloggable(state, world, pos, fluidState);
        }

        static boolean isStateFluidlogged(@Nonnull final Block fluid, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) {
            return FluidState.get(world, pos).getBlock() == fluid;
        }
    }
}
