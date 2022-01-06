package git.jbredwards.fluidlogged_api.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes fluid light level issues WIP
 * @author jbred
 *
 */
public final class ChunkPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //setBlockState, line 592
        if(checkMethod(method, obfuscated ? "func_177436_a" : "setBlockState", null))
            return 1;

        else return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(index == 1 && checkMethod(insn, "getLightOpacity", null)) {
            //instructions.insert(insn, genMethodNode("getLightOpacitySkipFluid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I"));
            //instructions.remove(insn);
            //return true;
        }

        return false;
    }
}
