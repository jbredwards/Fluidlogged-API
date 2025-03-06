/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.optifine;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumBlockRenderType;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * Set the block renderType (aka mc_Entity.y) to 1 if the block is a BlockLiquid
 * @author jbred
 *
 */
public final class PluginSVertexBuilder implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return "pushEntity".equals(method.name); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * pushEntity:
         * Old code:
         * int renderType = block.getRenderType(blockState).ordinal();
         *
         * New code:
         * // tell Optifine to pass the old render type for BlockLiquid blocks into shaders
         * int renderType = Hooks.getRenderType(block.getRenderType(blockState), blockState).ordinal();
         */
        if(checkMethod(insn, obfuscated ? "func_149645_b" : "getRenderType")) {
            instructions.insert(insn, genMethodNode("getRenderType", "(Lnet/minecraft/util/EnumBlockRenderType;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/util/EnumBlockRenderType;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static EnumBlockRenderType getRenderType(@Nonnull final EnumBlockRenderType oldType, @Nonnull final IBlockState state) {
            return state.getBlock() instanceof BlockLiquid ? EnumBlockRenderType.LIQUID : oldType;
        }
    }
}
