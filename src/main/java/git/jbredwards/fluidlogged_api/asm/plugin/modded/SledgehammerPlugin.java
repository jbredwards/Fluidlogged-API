package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;

/**
 * fixes serverside crash with sledgehammer installed
 * @author jbred
 *
 */
public class SledgehammerPlugin extends AbstractPlugin
{
    @Override
    public byte[] transform(byte[] basicClass, boolean obfuscated) { return ASMUtils.removeAnnotations(basicClass); }

    //unused
    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) { return false; }

    //unused
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) { return null; }

    //unused
    @Nullable
    @Override
    public String getMethodDesc() { return null; }
}
