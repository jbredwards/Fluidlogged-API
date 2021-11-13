package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * non-empty FluidState blocks call randomDisplayTick
 * @author jbred
 *
 */
public final class WorldClientPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //showBarrierParticles, line 391
        return checkMethod(method, obfuscated ? "func_184153_a" : "showBarrierParticles", null) ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_180655_c" : "randomDisplayTick", null)) {
            final InsnList list = new InsnList();
            //add params
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ILOAD, 1));
            list.add(new VarInsnNode(ILOAD, 2));
            list.add(new VarInsnNode(ILOAD, 3));
            list.add(new VarInsnNode(ILOAD, 4));
            list.add(new VarInsnNode(ALOAD, 5));
            list.add(new VarInsnNode(ALOAD, 7));

            list.add(genMethodNode("showBarrierParticles", "(Lnet/minecraft/client/multiplayer/WorldClient;IIIILjava/util/Random;Lnet/minecraft/util/math/BlockPos$MutableBlockPos;)V"));
            instructions.insert(insn, list);
            return true;
        }

        return false;
    }
}
