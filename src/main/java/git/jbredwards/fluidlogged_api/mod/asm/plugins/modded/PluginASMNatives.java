package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;
import java.util.function.Consumer;

/**
 * builds the internal ASMNatives functions
 * @author jbred
 *
 */
public final class PluginASMNatives implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.methods.removeIf(method -> (method.access & ACC_NATIVE) != 0);
        //Block
        addMethod(classNode, "getCanFluidFlow", "(Lnet/minecraft/block/Block;)Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", "canFluidFlow", "Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;");
        });
        addMethod(classNode, "setCanFluidFlow", "(Lnet/minecraft/block/Block;Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;)V", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/Block", "canFluidFlow", "Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;");
        });
        //BlockBush
        addMethod(classNode, "canSustainBush", "(Lnet/minecraft/block/BlockBush;Lnet/minecraft/block/state/IBlockState;)Z", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/block/BlockBush", "canSustainBush_Public", "(Lnet/minecraft/block/state/IBlockState;)Z", false);
        });
        //BlockFluidBase
        addMethod(classNode, "getFlowDecay", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidBase", "getFlowDecay_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", false);
        });
        //BlockFluidClassic
        addMethod(classNode, "getOptimalFlowDirections", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)[Z", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "getOptimalFlowDirections_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)[Z", false);
        });
        addMethod(classNode, "getLargerQuanta", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "getLargerQuanta_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", false);
        });
        addMethod(classNode, "canFlowInto", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "canFlowInto_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", false);
        });
        addMethod(classNode, "flowIntoBlock", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "flowIntoBlock_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", false);
        });
        //Fluid
        addMethod(classNode, "getDefaultFluidState", "(Lnet/minecraftforge/fluids/Fluid;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/Fluid", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });
        addMethod(classNode, "setDefaultFluidState", "(Lnet/minecraftforge/fluids/Fluid;Lgit/jbredwards/fluidlogged_api/api/util/FluidState;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitInsn(DUP_X1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraftforge/fluids/Fluid", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });
        //Template
        addMethod(classNode, "setKeepOldFluidStates", "(Lnet/minecraft/world/gen/structure/template/Template;Z)V", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ILOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/world/gen/structure/template/Template", "keepOldFluidStates", "Z");
        });

        return false;
    }

    void addMethod(@Nonnull ClassNode classNode, @Nonnull String name, @Nonnull String desc, @Nonnull Consumer<GeneratorAdapter> consumer) {
        final MethodNode method = new MethodNode(ACC_PUBLIC | ACC_STATIC, name, desc, null, null);
        informConsole(classNode.name, method);
        //write new body data
        consumer.accept(new GeneratorAdapter(method, method.access, method.name, method.desc));
        method.visitInsn(Type.getReturnType(method.desc).getOpcode(IRETURN));
        //add the newly generated method
        classNode.methods.add(method);
    }
}
