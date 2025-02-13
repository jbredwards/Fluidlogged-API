/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.optifine;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

/**
 * bring Optifine's IResolvable interface (from G5) to old Optifine versions (like F5)
 * - fixes crashes caused by Optifine loading classes too early
 * @author jbred
 *
 */
public final class PluginIResolvable implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("<init>"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        if(checkMethod(insn, "getTargetClass") || checkMethod(insn, "getTargetConstructor") || checkMethod(insn, "getTargetField") || checkMethod(insn, "getTargetMethod")) {
            instructions.insertBefore(insn, genMethodNode("register", withAccessorClass("(L%s;)V")));
            instructions.insertBefore(insn, new InsnNode(ACONST_NULL));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        if(!classNode.interfaces.contains("net/optifine/reflect/IResolvable")) {
            classNode.interfaces.add(getAccessorClass());
            addMethod(classNode, "fluidlogged_api$resolve", "()V", null, null, generator -> {
                switch(classNode.name) {
                    case "net/optifine/reflect/ReflectorClass": {
                        generator.visitVarInsn(ALOAD, 0);
                        generator.visitMethodInsn(INVOKEVIRTUAL, classNode.name, "getTargetClass", "()Ljava/lang/Class;", false);
                        generator.visitInsn(POP);
                        break;
                    }
                    case "net/optifine/reflect/ReflectorConstructor": {
                        generator.visitVarInsn(ALOAD, 0);
                        generator.visitMethodInsn(INVOKEVIRTUAL, classNode.name, "getTargetConstructor", "()Ljava/lang/reflect/Constructor;", false);
                        generator.visitInsn(POP);
                        break;
                    }
                    case "net/optifine/reflect/ReflectorField": {
                        generator.visitVarInsn(ALOAD, 0);
                        generator.visitMethodInsn(INVOKEVIRTUAL, classNode.name, "getTargetField", "()Ljava/lang/reflect/Field;", false);
                        generator.visitInsn(POP);
                        break;
                    }
                    case "net/optifine/reflect/ReflectorMethod": {
                        generator.visitVarInsn(ALOAD, 0);
                        generator.visitMethodInsn(INVOKEVIRTUAL, classNode.name, "getTargetMethod", "()Ljava/lang/reflect/Method;", false);
                        generator.visitInsn(POP);
                        break;
                    }
                }
            });

            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        private static List<Accessor> ACCESSORS = new ArrayList<>();

        public static void register(@Nonnull final Accessor accessor) {
            if(ACCESSORS == null) accessor.fluidlogged_api$resolve();
            else ACCESSORS.add(accessor);
        }

        // helper
        public static void resolve() {
            if(ACCESSORS != null) {
                ACCESSORS.forEach(Accessor::fluidlogged_api$resolve);
                ACCESSORS = null;
            }
        }
    }

    public interface Accessor
    {
        void fluidlogged_api$resolve();
    }
}
