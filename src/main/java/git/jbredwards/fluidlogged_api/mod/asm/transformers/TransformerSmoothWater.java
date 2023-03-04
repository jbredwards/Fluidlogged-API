package git.jbredwards.fluidlogged_api.mod.asm.transformers;

import net.minecraft.launchwrapper.IClassTransformer;
import org.objectweb.asm.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * revert super class changes made by SmoothWater
 * @author jbred
 *
 */
@SuppressWarnings("unused")
public final class TransformerSmoothWater implements IClassTransformer
{
    @Nullable
    @Override
    public byte[] transform(@Nonnull String name, @Nonnull String transformedName, @Nullable byte[] basicClass) {
        if(basicClass == null) return null;
        final ClassReader reader = new ClassReader(basicClass);
        if(isSmoothWaterClass(reader.getSuperName())) {
            final ClassWriter writer = new ClassWriter(0);
            reader.accept(new ClassVisitor(Opcodes.ASM5, writer) {
                @Override
                public void visit(int version, int access, @Nonnull String name, @Nonnull String signature, @Nonnull String superName, @Nonnull String[] interfaces) {
                    super.visit(version, access, name, signature, "net/minecraft/block/BlockLiquid", interfaces);
                }

                @Nonnull
                @Override
                public MethodVisitor visitMethod(int access, @Nonnull String name, @Nonnull String desc, @Nonnull String signature, @Nonnull String[] exceptions) {
                    final MethodVisitor old = super.visitMethod(access, name, desc, signature, exceptions);
                    return "<init>".equals(name) ? new MethodVisitor(Opcodes.ASM5, old) {
                        @Override
                        public void visitMethodInsn(int opcode, @Nonnull String owner, @Nonnull String name, @Nonnull String desc, boolean itf) {
                            super.visitMethodInsn(opcode, isSmoothWaterClass(owner) ? "net/minecraft/block/BlockLiquid" : owner, name, desc, itf);
                        }
                    } : old;
                }
            }, 0);

            return writer.toByteArray();
        }

        return basicClass;
    }

    static boolean isSmoothWaterClass(@Nonnull String clazz) {
        return "pl/asie/smoothwater/BlockLiquidForged".equals(clazz) || "pl/asie/mage/core/water/BlockLiquidForged".equals(clazz);
    }
}
