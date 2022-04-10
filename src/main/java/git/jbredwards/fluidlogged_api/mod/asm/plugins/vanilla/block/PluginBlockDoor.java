package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * makes doors fluidloggable by default
 * @author jbred
 *
 */
public final class PluginBlockDoor implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return method.name.equals(obfuscated ? "func_180639_a" : "onBlockActivated")
                || method.name.equals(obfuscated ? "func_176512_a" : "toggleDoor")
                || method.name.equals(obfuscated ? "func_189540_a" : "neighborChanged");
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //update upper FluidState when the door opens or closes
        if(checkMethod(insn, obfuscated ? "func_175704_b" : "markBlockRangeForRenderUpdate", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V")) {
            instructions.insert(insn, genMethodNode("notifyDoorFluids", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        //ensure FluidStates only flow from certain sides
        addMethod(classNode, "canFluidFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z",
            "canDoorFluidFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitMaxs(4, 0);
            }
        );

        return true;
    }
}
