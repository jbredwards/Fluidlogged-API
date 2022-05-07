package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * modded fluids work properly with the mod
 * @author jbred
 *
 */
public final class PluginBlockFluidClassic implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        //add public getter for protected method
        addMethod(classNode, "getLargerQuanta_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", "getLargerQuanta", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", false);
            generator.visitMaxs(4, 0);
        });
        //add public getter for protected method
        addMethod(classNode, "calculateFlowCost_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;II)I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitVarInsn(ILOAD, 4);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", "calculateFlowCost", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;II)I", false);
            generator.visitMaxs(5, 0);
        });
        //add public getter for protected method
        addMethod(classNode, "flowIntoBlock_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", "flowIntoBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", false);
            generator.visitMaxs(4, 0);
        });

        return false;
    }
}
