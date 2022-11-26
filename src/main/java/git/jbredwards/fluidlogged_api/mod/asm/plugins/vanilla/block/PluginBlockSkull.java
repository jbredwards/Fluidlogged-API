package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * wither skulls no longer void the FluidState here when summoning the wither
 * lots of people totally knew this was a problem, trust me ;)
 * @author jbred
 *
 */
public final class PluginBlockSkull implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_176500_f" : "checkWitherSpawn"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * checkWitherSpawn: (changes are around line 245)
         * Old code:
         * worldIn.setBlockState(blockworldstate1.getPos(), Blocks.AIR.getDefaultState(), 2);
         *
         * New code:
         * worldIn.setBlockState(blockworldstate1.getPos(), FluidState.get(worldIn, pos).getState(), 2);
         */
        if(checkField(insn, obfuscated ? "field_150350_a" : "AIR", "Lnet/minecraft/block/Block;")) {
            final InsnList list = new InsnList();
            //parameters
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            //adds new code
            list.add(genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidState", "get", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getState", "()Lnet/minecraft/block/state/IBlockState;", false));
            instructions.insertBefore(insn, list);
            removeFrom(instructions, insn, 1);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        return true;
    }
}
