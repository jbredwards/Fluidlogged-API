package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * makes trap doors fluidloggable by default
 * @author jbred
 *
 */
public final class PluginBlockTrapDoor implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_185731_a" : "playSound"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //update fluids here when trapdoors open & close
        instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
        instructions.insertBefore(insn, new VarInsnNode(ALOAD, 3));
        instructions.insertBefore(insn, genMethodNode("notifyFluidStates", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V"));
        return true;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        addMethod(classNode, "canFluidFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z",
            "canTrapDoorFluidFlow", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitMaxs(2, 0);
            }
        );

        return true;
    }
}
