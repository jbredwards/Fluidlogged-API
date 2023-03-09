package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * store one FluidState inside each BlockStateBase instance, this greatly increases the speed of fluid logic
 * @author jbred
 *
 */
public final class PluginBlockStateBase implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("<init>"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * Constructor:
         * New code:
         * //initialize defaultFluidState
         * public BlockStateBase() { defaultFluidState = FluidState.EMPTY; }
         */
        if(insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETSTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "EMPTY", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            instructions.insertBefore(insn, new FieldInsnNode(PUTFIELD, "net/minecraft/block/state/BlockStateBase", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
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
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/asm/impl/IFluidStateProvider");
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
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/state/BlockStateBase", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
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
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/state/BlockStateBase", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });

        return true;
    }
}
