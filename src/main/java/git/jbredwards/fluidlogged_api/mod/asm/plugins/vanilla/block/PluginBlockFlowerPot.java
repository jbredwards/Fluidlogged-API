/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * fix FluidState voiding if a fluidlogged flower pot is removed
 * @author jbred
 *
 */
public final class PluginBlockFlowerPot implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals(obfuscated ? "func_180657_a" : "harvestBlock"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * harvestBlock: (changes are around line 485)
         * Old code:
         * world.setBlockToAir(pos);
         *
         * New code
         * // only remove the block here if it has yet to be removed
         * Hooks.setFlowerPotToAir(world, pos, state);
         */
        if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 4));
            instructions.insertBefore(insn, genMethodNode("setFlowerPotToAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)V"));
            removeFrom(instructions, insn, 1);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void setFlowerPotToAir(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state) {
            @Nonnull final Chunk chunk = world.getChunk(pos);
            if(chunk.getBlockState(pos) == state) {
                world.setBlockState(pos, FluidState.getFromProvider(chunk, pos).toFlowing().getState(),
                        world.isRemote ? Constants.BlockFlags.DEFAULT_AND_RERENDER : Constants.BlockFlags.DEFAULT);
            }
        }
    }
}
