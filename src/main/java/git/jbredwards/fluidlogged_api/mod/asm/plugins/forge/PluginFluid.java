package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
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
        /*
         * Constructor: (changes are around line 171)
         * Old code:
         * {
         *     this.fluidName = fluidName.toLowerCase(Locale.ENGLISH);
         *     ...
         * }
         *
         * New code:
         * //initializes new defaultFluidState field with an empty value
         * //(it will be updated later, after the fluid is assigned a block)
         * {
         *     this.fluidName = fluidName.toLowerCase(Locale.ENGLISH);
         *     this.defaultFluidState = FluidState.EMPTY;
         *     ...
         * }
         */
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
        /*
         * =========
         * Accessors
         * =========
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginFluid$Accessor");
        /*
         * Accessor:
         * New code:
         * //getter for defaultFluidState
         * @ASMGenerated
         * public FluidState getDefaultFluidState()
         * {
         *     return this.defaultFluidState;
         * }
         */
        addMethod(classNode, "getDefaultFluidState", "()Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/Fluid", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });
        /*
         * Accessor:
         * New code:
         * //setter for defaultFluidState
         * @ASMGenerated
         * public void setDefaultFluidState(@Nonnull FluidState fluidState)
         * {
         *     this.defaultFluidState = fluidState;
         * }
         */
        addMethod(classNode, "setDefaultFluidState", "(Lgit/jbredwards/fluidlogged_api/api/util/FluidState;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraftforge/fluids/Fluid", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });

        return true;
    }

    public interface Accessor
    {
        @Nonnull
        FluidState getDefaultFluidState();
        void setDefaultFluidState(@Nonnull FluidState fluidState);
    }
}
