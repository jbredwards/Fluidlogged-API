package git.jbredwards.fluidlogged_api.asm.plugin;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * removes water & lava from the built-in blocks list
 * @author jbred
 *
 */
public final class BlockModelShapesPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "<init>";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/client/renderer/block/model/ModelManager;)V";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_178119_d" : "registerAllBlocks", "()V")) {
            instructions.insert(insn, method("registerBuiltinBlocks", "(Lnet/minecraft/client/renderer/BlockModelShapes;)V"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
        }

        return false;
    }
}
