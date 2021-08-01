package git.jbredwards.fluidlogged_api.asm;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 * some helpful functions for asm class transformers
 * @author jbred
 *
 */
public enum ASMUtils
{
    ;

    //same as the normal method, but this one can specify how many to go back
    @Nonnull
    public static AbstractInsnNode getPrevious(@Nonnull AbstractInsnNode insn, int count) {
        for(int i = 0; i < count; i++) insn = Objects.requireNonNull(insn.getPrevious());
        return insn;
    }

    //same as the normal method, but this one can specify how many to go forward
    @Nonnull
    public static AbstractInsnNode getNext(@Nonnull AbstractInsnNode insn, int count) {
        for(int i = 0; i < count; i++) insn = Objects.requireNonNull(insn.getNext());
        return insn;
    }

    //returns true if the insn is both a method and if it matches the name & desc
    public static boolean checkMethod(@Nullable AbstractInsnNode insn, @Nullable String name, @Nullable String desc) {
        //dude it isn't even a method...
        if(!(insn instanceof MethodInsnNode)) return false;
        //if both are null, assume looking for any method
        else if(name == null && desc == null) return true;
        //if name null, assume only looking for desc
        else if(name == null) return ((MethodInsnNode)insn).desc.equals(desc);
        //if desc null, assume only looking for name
        else if(desc == null) return ((MethodInsnNode)insn).name.equals(name);
        //default return
        else return ((MethodInsnNode)insn).name.equals(name) && ((MethodInsnNode)insn).desc.equals(desc);
    }

    //same as above, but for method nodes
    public static boolean checkMethod(@Nonnull MethodNode method, @Nullable String name, @Nullable String desc) {
        //if both are null, assume looking for any method
        if(name == null && desc == null) return true;
        //if name null, assume only looking for desc
        else if(name == null) return method.desc.equals(desc);
        //if desc null, assume only looking for name
        else if(desc == null) return method.name.equals(name);
        //default return
        else return method.name.equals(name) && method.desc.equals(desc);
    }

    //returns true if the insn is both a field and if it matches the name & desc
    public static boolean checkField(@Nullable AbstractInsnNode insn, @Nullable String name, @Nullable String desc) {
        //not a field
        if(!(insn instanceof FieldInsnNode)) return false;
        //if all are null, assume looking for any field
        else if(name == null && desc == null) return true;
        //only looking for desc
        else if(name == null) return ((FieldInsnNode)insn).desc.equals(desc);
        //only looking for name
        else if(desc == null) return ((FieldInsnNode)insn).name.equals(name);
        //default
        else return((FieldInsnNode)insn).name.equals(name) && ((FieldInsnNode)insn).desc.equals(desc);
    }

    //removes all annotations from a class during runtime
    @Nonnull
    public static byte[] removeAnnotations(@Nonnull byte[] basicClass) {
        final ClassReader reader = new ClassReader(basicClass);
        final ClassWriter writer = new ClassWriter(0);
        //removes all annotations from the class
        reader.accept(new ClassVisitor(Opcodes.ASM5, writer) {
            //removes all class annotations
            public AnnotationVisitor visitAnnotation(String desc, boolean visible) { return null; }
            //removes all field annotations
            public FieldVisitor visitField(int access, String name, String desc, String signature, Object value) {
                return new FieldVisitor(Opcodes.ASM5, cv.visitField(access, name, desc, signature, value)) {
                    public AnnotationVisitor visitAnnotation(String desc, boolean visible) { return null; }
                };
            }
            //removes all method annotations
            public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
                return new MethodVisitor(Opcodes.ASM5, cv.visitMethod(access, name, desc, signature, exceptions)) {
                    public AnnotationVisitor visitAnnotation(String desc, boolean visible) { return null; }
                };
            }
        }, 0);
        //returns the transformed class
        return writer.toByteArray();
    }
}
