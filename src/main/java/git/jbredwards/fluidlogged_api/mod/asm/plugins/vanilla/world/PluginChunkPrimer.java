package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * allow mods to generate FluidStates more optimally during world gen
 * @author jbred
 *
 */
public final class PluginChunkPrimer implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("<init>"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //call super on correct class
        if(checkMethod(insn, "<init>")) {
            ((MethodInsnNode)insn).owner = "git/jbredwards/fluidlogged_api/api/asm/impl/IFluidStatePrimer";
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.superName = "git/jbredwards/fluidlogged_api/api/asm/impl/IFluidStatePrimer";
        return true;
    }
}
