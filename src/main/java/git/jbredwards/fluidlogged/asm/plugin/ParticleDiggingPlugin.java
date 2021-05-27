package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;


/**
 * applies the new IParticleColor interface
 * @author jbred
 *
 */
public final class ParticleDiggingPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_187154_b" : "multiplyColor";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/util/math/BlockPos;)V";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKEVIRTUAL && insn instanceof MethodInsnNode && ((MethodInsnNode)insn).owner.equals("net/minecraft/client/renderer/color/BlockColors")) {
            //adds the new code
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/ASMHooks", "getColor",
                    "(Lnet/minecraft/client/renderer/color/BlockColors;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)I", false));
            //removes the old code
            instructions.remove(insn);

            return true;
        }

        return false;
    }
}
