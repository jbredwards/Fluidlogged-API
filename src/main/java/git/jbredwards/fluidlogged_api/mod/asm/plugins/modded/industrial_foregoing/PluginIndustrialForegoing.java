/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.industrial_foregoing;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fluids.FluidStack;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * make industrial foregoing's fluid pump FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginIndustrialForegoing implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("work"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * work:
         * Old code:
         * while (!allBlocks.isEmpty() && (this.world.isOutsideBuildHeight(peeked) || !isBlockSameFluid(peeked) || this.world.getBlockState(peeked).getBlock().getMetaFromState(this.world.getBlockState(peeked)) != 0))
         * {
         *     ...
         * }
         *
         * New code:
         * // account for FluidStates
         * while (!allBlocks.isEmpty() && (this.world.isOutsideBuildHeight(peeked) || !isBlockSameFluid(peeked) || FluidloggedUtils.getFluidState(this.world, peeked).getLevel() != 0))
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, obfuscated ? "func_176201_c" : "getMetaFromState")) {
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getLevel", "()I", false));
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            removeFrom(instructions, insn, -6);
        }
        /*
         * work:
         * Old code:
         * if (this.world.getTileEntity(peeked) != null) return 0;
         *
         * New code:
         * // tile entities are fluidloggable, so don't skip the drain if a tile entity is at the position
         * if (null != null) return 0;
         */
        else if(checkMethod(insn, obfuscated ? "func_175625_s" : "getTileEntity")) {
            instructions.insert(insn, new InsnNode(ACONST_NULL));
            removeFrom(instructions, insn, -3);
        }
        /*
         * work:
         * Old code:
         * if (BlockRegistry.fluidPumpBlock.isReplaceFluidWithCobble())
         * {
         *     ...
         * }
         *
         * New code:
         * // check if pos is replaceable before setting cobble
         * if (Hooks.isReplaceable(this, peeked, BlockRegistry.fluidPumpBlock.isReplaceFluidWithCobble()))
         * {
         *     ...
         * }
         */
        else if(checkMethod(insn, "isReplaceFluidWithCobble")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, genMethodNode("isReplaceable", "(Lnet/minecraft/tileentity/TileEntity;Lnet/minecraft/util/math/BlockPos;Z)Z"));
        }
        /*
         * work:
         * Old code:
         * else if (world.setBlockToAir(peeked))
         * {
         *     ...
         * }
         *
         * New code:
         * // don't set the block to air (this was handled by the prior call to IFluidHandler#drain)
         * else if (true)
         * {
         *     ...
         * }
         */
        else if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
            instructions.insert(insn, new InsnNode(ICONST_1));
            removeFrom(instructions, insn, -3);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * isBlockSameFluid:
         * Old code:
         * private boolean isBlockSameFluid(BlockPos pos)
         * {
         *     ...
         * }
         *
         * New code:
         * // account for FluidStates
         * private boolean isBlockSameFluid(BlockPos pos)
         * {
         *     return Hooks.isSameFluid(this, this.fluid, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("isBlockSameFluid"),
            "isSameFluid", "(Lnet/minecraft/tileentity/TileEntity;Ljava/lang/String;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "com/buuz135/industrial/tile/world/FluidPumpTile", "fluid", "Ljava/lang/String;");
                generator.visitVarInsn(ALOAD, 1);
            }
        );

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isReplaceable(@Nonnull final TileEntity tile, @Nonnull final BlockPos pos, final boolean flag) {
            return flag && tile.getWorld().getBlockState(pos).getBlock().isReplaceable(tile.getWorld(), pos);
        }

        public static boolean isSameFluid(@Nonnull final TileEntity tile, @Nonnull final String fluid, @Nonnull final BlockPos pos) {
            @Nonnull final FluidState fluidState = FluidloggedUtils.getFluidState(tile.getWorld(), pos);
            if(!fluidState.isValid() || !fluidState.getFluid().getName().equals(fluid)) return false;

            @Nullable final FluidStack simDrain = fluidState.getFluidBlock().drain(tile.getWorld(), pos, false);
            return simDrain != null && simDrain.amount == 1000;
        }
    }
}
