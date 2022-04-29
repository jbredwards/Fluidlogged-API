package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * Non-empty FluidStates call randomDisplayTick & move hardcoded barrier stuff to barrier.randomDisplayTick
 * @author jbred
 *
 */
public final class PluginWorldClient implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_73029_E" : "doVoidFogParticles")) return 1;
        else if(method.name.equals(obfuscated ? "func_184153_a" : "showBarrierParticles")) return 2;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //boost performance a tad cause why nod
        if(index == 1 && checkMethod(insn, obfuscated ? "func_184614_ca" : "getHeldItemMainhand")) {
            instructions.insert(insn, new FieldInsnNode(GETSTATIC, "net/minecraft/item/ItemStack", obfuscated ? "field_190927_a" : "EMPTY", "Lnet/minecraft/item/ItemStack;"));
            removeFrom(instructions, insn, -3);
            return true;
        }
        //non-empty FluidStates call randomDisplayTick
        else if(checkMethod(insn, obfuscated ? "func_180655_c" : "randomDisplayTick", null)) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ILOAD, 1));
            list.add(new VarInsnNode(ILOAD, 2));
            list.add(new VarInsnNode(ILOAD, 3));
            list.add(new VarInsnNode(ILOAD, 4));
            list.add(new VarInsnNode(ALOAD, 5));
            list.add(genMethodNode("randomFluidStateTick", "(Lnet/minecraft/world/World;IIIILjava/util/Random;)V"));
            instructions.insert(insn, list);
            return true;
        }

        return false;
    }
}
