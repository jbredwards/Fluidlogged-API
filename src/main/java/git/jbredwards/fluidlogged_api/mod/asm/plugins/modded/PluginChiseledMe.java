package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix chiseled me conflict
 * @author jbred
 *
 */
public final class PluginChiseledMe implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("travel"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        method.visibleAnnotations.clear();
        /*
         * travel:
         * Old code:
         * @ModifyConstant(method = "travel", constant = @Constant(doubleValue = 0.6000000238418579))
         * double travel(double constant)
         * {
         *     ...
         * }
         *
         * New code:
         * //change constant to be compatible with this mod
         * @ModifyConstant(method = "travel", constant = @Constant(doubleValue = 0.100001))
         * double travel(double constant)
         * {
         *     ...
         * }
         */
        //Most of this code is auto-generated
        final AnnotationVisitor av0 = method.visitAnnotation("Lorg/spongepowered/asm/mixin/injection/ModifyConstant;", true);
        {
            final AnnotationVisitor av1 = av0.visitArray("method");
            av1.visit(null, "travel");
            av1.visitEnd();
        }
        {
            final AnnotationVisitor av1 = av0.visitArray("constant");
            {
                final AnnotationVisitor av2 = av1.visitAnnotation(null, "Lorg/spongepowered/asm/mixin/injection/Constant;");
                av2.visit("doubleValue", new Double("0.100001"));
                av2.visitEnd();
            }
            av1.visitEnd();
        }
        av0.visitEnd();
        return true;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        //fluidlogged api already implements these patches
        classNode.methods.removeIf(method -> method.name.equals("rayTraceBlocks") || method.name.equals("isInLava"));
        return classNode.name.equals("dev/necauqua/mods/cm/mixin/entity/EntityLivingBaseMixin");
    }
}
