package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public class BlockPlugin extends AbstractPlugin
{
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_189539_a" : "eventReceived";
    }

    @Nullable
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;II)Z";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == ICONST_0) {
            instructions.insert(method("eventReceived", "(I)Z"));
            instructions.insert(new VarInsnNode(ILOAD, 4));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
