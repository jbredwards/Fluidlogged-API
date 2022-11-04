package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.state.IBlockState;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * breaking this block type no longer voids the possible FluidState here
 * @author jbred
 *
 */
public final class PluginBlockBush implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_176475_e" : "checkAndDropBlock"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * checkAndDropBlock: (changes are around line 79)
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

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * =========
         * Accessors
         * =========
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/block/PluginBlockBush$Accessor");
        /*
         * Accessor:
         * New code:
         * //add public getter for protected method
         * @ASMGenerated
         * public boolean canSustainBush_Public(IBlockState state)
         * {
         *     return this.canSustainBush(state);
         * }
         */
        addMethod(classNode, "canSustainBush_Public", "(Lnet/minecraft/block/state/IBlockState;)Z", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/block/BlockBush", obfuscated ? "func_185514_i" : "canSustainBush", "(Lnet/minecraft/block/state/IBlockState;)Z", false);
        });

        return true;
    }

    public interface Accessor
    {
        boolean canSustainBush_Public(@Nonnull IBlockState state);
    }
}
