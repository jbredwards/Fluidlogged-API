package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.material.Material;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * farmland blocks now recognise water FluidStates
 * @author jbred
 *
 */
public final class PluginBlockFarmland implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_176530_e" : "hasWater"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * hasWater: (changes are around line 119)
         * Old code:
         * if (worldIn.getBlockState(blockpos$mutableblockpos).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         *
         * New code:
         * //check for fluidlogged blocks
         * if (Hooks.getWaterBlock(worldIn, blockpos$mutableblockpos) == Material.WATER)
         * {
         *     ...
         * }
         */
        //separate obfuscated check to resolve foamfix conflict
        if(obfuscated && checkMethod(insn, "func_180495_p") || checkMethod(insn, "getBlockState")) {
            instructions.insertBefore(insn, genMethodNode("getWaterBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/material/Material;"));
            removeFrom(instructions, insn, 1);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static Material getWaterBlock(@Nonnull World world, @Nonnull BlockPos pos) {
            final Chunk chunk = world.getChunkProvider().getLoadedChunk(pos.getX() >> 4, pos.getZ() >> 4);
            if(chunk == null) return Material.AIR;

            return chunk.getBlockState(pos).getMaterial() == Material.WATER ? Material.WATER
                    : FluidState.getFromProvider(chunk, pos).getMaterial();
        }
    }
}
