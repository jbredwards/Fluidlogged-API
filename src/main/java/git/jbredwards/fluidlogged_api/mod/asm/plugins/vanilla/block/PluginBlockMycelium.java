package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * use World#getBlockLightOpacity for FluidState sensitivity
 * @author jbred
 *
 */
public final class PluginBlockMycelium implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_180650_b" : "updateTick"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, "getLightOpacity")) {
            /*
             * updateTick (changes are around line 47):
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
                removeFrom(instructions, insn, -4);
            }
            /*
             * updateTick (changes are around line 61):
             * Old code:
             * if (iblockstate.getBlock() == Blocks.DIRT && iblockstate.getValue(BlockDirt.VARIANT) == BlockDirt.DirtType.DIRT && worldIn.getLightFromNeighbors(blockpos.up()) >= 4 && iblockstate1.getLightOpacity(worldIn, pos.up()) <= 2)
             * {
             *     ...
             * }
             *
             * New code:
             * //use World#getBlockLightOpacity for FluidState sensitivity
             * if (iblockstate.getBlock() == Blocks.DIRT && iblockstate.getValue(BlockDirt.VARIANT) == BlockDirt.DirtType.DIRT && worldIn.getLightFromNeighbors(blockpos.up()) >= 4 && worldIn.getBlockLightOpacity(pos.up()) <= 2)
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
         * updateTick (changes are around line 59):
         * Old code:
         * IBlockState iblockstate1 = worldIn.getBlockState(blockpos.up());
         *
         * New code:
         * //this variable is no longer used, change to null to improve performance
         * IBlockState iblockstate1 = null;
         */
        else if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState") && insn.getPrevious().getOpcode() == INVOKEVIRTUAL && insn.getNext().getOpcode() != INVOKEINTERFACE) {
            instructions.insert(insn, new InsnNode(ACONST_NULL));
            removeFrom(instructions, insn, -3);
        }

        return false;
    }
}
