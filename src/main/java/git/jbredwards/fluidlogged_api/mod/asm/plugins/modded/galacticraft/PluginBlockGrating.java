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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.galacticraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import micdoodle8.mods.galacticraft.core.blocks.BlockGrating;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraftforge.fluids.BlockFluidBase;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * make Galacticraft's grating block use actual fluidlogging, instead of pseudo fluidlogging
 * @author jbred
 *
 */
public final class PluginBlockGrating implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("createForgeFluidVersion"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * createForgeFluidVersion:
         * Old code:
         * blockRegistry.register(grating);
         *
         * New code:
         * // Don't register fluid grating blocks, so they can be remapped
         * ...
         */
        if(checkMethod(insn, "register")) {
            removeFrom(instructions, insn, -2);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.methods.removeIf(method
                -> method.name.equals("canRenderInLayer")
                || method.name.equals("getExtendedState")
                || method.name.equals("getStateForPlacement")
                || method.name.equals("removedByPlayer")
                || method.name.equals(obfuscated ? "func_180663_b" : "breakBlock")
                || method.name.equals(obfuscated ? "func_180660_a" : "getItemDropped")
                || method.name.equals(obfuscated ? "func_149688_o" : "getMaterial")
                || method.name.equals(obfuscated ? "func_176203_a" : "getStateFromMeta")
                || method.name.equals(obfuscated ? "func_189540_a" : "neighborChanged")
                || method.name.equals(obfuscated ? "func_176213_c" : "onBlockAdded")
                || method.name.equals(obfuscated ? "func_149738_a" : "tickRate")
                || method.name.equals(obfuscated ? "func_180650_b" : "updateTick"));
        /*
         * New code:
         * // Don't use a custom block state container class
         * @ASMOverwrite
         * protected BlockStateContainer createBlockState()
         * {
         *     return Hooks.createBlockState(this);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180661_e" : "createBlockState"),
                "createBlockState", "(Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/BlockStateContainer;", generator -> generator.visitVarInsn(ALOAD, 0));
        /*
         * New code:
         * // LEVEL property is now unused, so don't save it
         * @ASMOverwrite
         * public int getMetaFromState(IBlockState state)
         * {
         *     return 0;
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176201_c" : "getMetaFromState"), null, null, generator -> generator.visitInsn(ICONST_0));
        // make grating override the "applyDefaults" setting, as it must be fluidloggable for its own functionality
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        addMethod(classNode, "overrideApplyDefaultsSetting", "()Z", null, null, generator -> generator.visitInsn(ICONST_1));
        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static BlockStateContainer createBlockState(@Nonnull final Block grating) {
            return new BlockStateContainer.Builder(grating).add(BlockLiquid.LEVEL, BlockFluidBase.LEVEL).add(BlockGrating.BASE_STATE).build();
        }
    }
}
