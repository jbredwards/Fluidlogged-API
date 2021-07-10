package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fixes serverside crash with sledgehammer installed
 * @author jbred
 *
 */
public class SledgehammerPlugin extends AbstractPlugin
{
    @Override
    public byte[] transform(byte[] basicClass, boolean obfuscated) {
        final ClassReader reader = new ClassReader(basicClass);
        final ClassWriter writer = new ClassWriter(0);

        //changes the annotation value
        reader.accept(new ClassVisitor(Opcodes.ASM5, writer) {
            @Override
            public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
                return new AnnotationSwapper(cv.visitAnnotation(desc, visible));
            }
        }, 0);

        //returns the transformed class
        return writer.toByteArray();
    }

    //changes the annotation value
    public static class AnnotationSwapper extends AnnotationVisitor
    {
        public AnnotationSwapper(@Nonnull AnnotationVisitor av) {
            super(Opcodes.ASM5, av);
        }

        @Override
        public void visit(String name, Object value) {
            av.visit(name, Type.getType("Lgit/jbredwards/fluidlogged_api/asm/swapper/BlockLiquidBase;"));
        }

        @Override
        public AnnotationVisitor visitArray(String name) {
            return new AnnotationSwapper(super.visitArray(name));
        }
    }

    //unused
    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        return false;
    }

    //unused
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) {
        return null;
    }

    //unused
    @Nullable
    @Override
    public String getMethodDesc() {
        return null;
    }
}
