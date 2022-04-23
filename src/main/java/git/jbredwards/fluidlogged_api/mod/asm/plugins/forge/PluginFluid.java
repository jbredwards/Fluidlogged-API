package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * store one internal FluidState for each Fluid, as to decrease ram usage
 * @author jbred
 *
 */
public final class PluginFluid implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, "<init>", "(Ljava/lang/String;Lnet/minecraft/util/ResourceLocation;Lnet/minecraft/util/ResourceLocation;Lnet/minecraft/util/ResourceLocation;)V");
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(insn.getOpcode() == PUTFIELD) {
            instructions.insert(insn, new FieldInsnNode(PUTFIELD, "net/minecraftforge/fluids/Fluid", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            instructions.insert(insn, new FieldInsnNode(GETSTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "EMPTY", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null));
        return true;
    }
}
