package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * breaking this block type no longer voids the possible FluidState here
 * @author jbred
 *
 */
public final class PluginBlockCocoa implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_176500_f" : "dropBlock"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * dropBlock: (changes are around line 164)
         * Old code:
         * worldIn.setBlockState(pos, Blocks.AIR.getDefaultState(), 3);
         *
         * New code:
         * //when a block is removed by a player, set the FluidState here (if it's empty air is set)
         * worldIn.setBlockState(pos, FluidState.get(worldIn, pos).getState(), 3);
         */
        if(checkField(insn, obfuscated ? "field_150350_a" : "AIR", "Lnet/minecraft/block/Block;")) {
            final InsnList list = new InsnList();
            //parameters
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            //adds new code
            instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidState", "get", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getState", "()Lnet/minecraft/block/state/IBlockState;", false));
            removeFrom(instructions, insn, 1);
            return true;
        }

        return false;
    }
}
