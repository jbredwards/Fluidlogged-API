package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;

/**
 * registers this mod's built-in models
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
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKESPECIAL && insn instanceof MethodInsnNode && ((MethodInsnNode)insn).owner.equals("net/minecraft/client/renderer/BlockModelShapes")) {
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/ASMHooks", "registerBuiltinBlocks", "(Lnet/minecraft/client/renderer/BlockModelShapes;)V", false));

            instructions.insert(insn, list);
        }

        return false;
    }
}
