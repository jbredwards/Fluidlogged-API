package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * use World#getBlockLightOpacity for FluidState sensitivity
 * @author jbred
 *
 */
public final class PluginBlockGrass implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_180650_b" : "updateTick"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, "getLightOpacity")) {
            /*
             * updateTick (changes are around line 46):
             * Old code:
             * if (worldIn.getLightFromNeighbors(pos.up()) < 4 && worldIn.getBlockState(pos.up()).getLightOpacity(worldIn, pos.up()) > 2)
             * {
             *     ...
             * }
             *
             * New code:
             * //use World#getBlockLightOpacity for FluidState sensitivity
             * if (worldIn.getLightFromNeighbors(pos.up()) < 4 && worldIn.getBlockLightOpacity(pos.up()) > 2)
             * {
             *     ...
             * }
             */
            if(getPrevious(insn, 4).getOpcode() == INVOKEVIRTUAL) {
                instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", "getBlockLightOpacity", "(Lnet/minecraft/util/math/BlockPos;)I", false));
                removeFrom(instructions, insn, 4);
            }
            /*
             * updateTick (changes are around line 66):
             * Old code:
             * if (iblockstate1.getBlock() == Blocks.DIRT && iblockstate1.getValue(BlockDirt.VARIANT) == BlockDirt.DirtType.DIRT && worldIn.getLightFromNeighbors(blockpos.up()) >= 4 && iblockstate.getLightOpacity(worldIn, pos.up()) <= 2)
             * {
             *     ...
             * }
             *
             * New code:
             * //use World#getBlockLightOpacity for FluidState sensitivity
             * if (iblockstate1.getBlock() == Blocks.DIRT && iblockstate1.getValue(BlockDirt.VARIANT) == BlockDirt.DirtType.DIRT && worldIn.getLightFromNeighbors(blockpos.up()) >= 4 && worldIn.getBlockLightOpacity(pos.up()) <= 2)
             * {
             *     ...
             * }
             */
            else {
                instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", "getBlockLightOpacity", "(Lnet/minecraft/util/math/BlockPos;)I", false));
                instructions.remove(getPrevious(insn, 4));
                instructions.remove(insn);
                return true;
            }
        }
        /*
         * updateTick (changes are around line 63):
         * Old code:
         * IBlockState iblockstate = worldIn.getBlockState(blockpos.up());
         *
         * New code:
         * //this variable is no longer used, change to null to improve performance
         * IBlockState iblockstate = null;
         */
        else if(insn.getNext().getOpcode() == ASTORE && ((VarInsnNode)insn.getNext()).var == 7) {
            instructions.insert(insn, new InsnNode(ACONST_NULL));
            removeFrom(instructions, insn, 3);
        }

        return false;
    }
}
