package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix particle lighting while within fluidlogged blocks
 * @author jbred
 *
 */
public final class PluginParticle implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_189214_a" : "getBrightnessForRender"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_175626_b" : "getCombinedLight")) {
            instructions.insert(insn, genMethodNode("fixParticleBrightness", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I"));
            removeFrom(instructions, insn, -1);
            return true;
        }

        return false;
    }
}
