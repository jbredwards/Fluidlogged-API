package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix mixin annotation to target fluidlogged api transform
 * @author jbred
 *
 */
public final class PluginCubicChunks implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("getLightForExt_getMinHeight"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getLightForExt_getMinHeight:
         * Old code:
         * @ModifyConstant(method = "getLightForExt",
         *         constant = @Constant(intValue = 0, expandZeroConditions = Constant.Condition.GREATER_THAN_OR_EQUAL_TO_ZERO),
         *         slice = @Slice(
         *                 from = @At(value = "INVOKE:FIRST", target = "Lnet/minecraft/util/math/BlockPos;getY()I"),
         *                 to = @At(value = "INVOKE:FIRST",
         *                         target = "Lnet/minecraft/world/ChunkCache;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")
         *         )
         * )
         *
         * New code:
         * //target changed method instead of old
         * @ModifyConstant(method = "getLightForExt",
         *         constant = @Constant(intValue = 0, expandZeroConditions = Constant.Condition.GREATER_THAN_OR_EQUAL_TO_ZERO),
         *         slice = @Slice(
         *                 from = @At(value = "INVOKE:FIRST", target = "Lnet/minecraft/util/math/BlockPos;getY()I"),
         *                 to = @At(value = "INVOKE:FIRST",
         *                         target = "Lgit/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/world/PluginChunkCache$Hooks;useNeighborBrightness(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z",
         *                         remap = false)
         *         )
         * )
         */
        //Most of this code is auto generated
        method.visibleAnnotations.clear();
        final AnnotationVisitor av0 = method.visitAnnotation("Lorg/spongepowered/asm/mixin/injection/ModifyConstant;", true);
        {
            AnnotationVisitor av1 = av0.visitArray("method");
            av1.visit(null, "getLightForExt");
            av1.visitEnd();
        }
        {
            final AnnotationVisitor av1 = av0.visitArray("constant");
            {
                final AnnotationVisitor av2 = av1.visitAnnotation(null, "Lorg/spongepowered/asm/mixin/injection/Constant;");
                av2.visit("intValue", 0);
                {
                    final AnnotationVisitor av3 = av2.visitArray("expandZeroConditions");
                    av3.visitEnum(null, "Lorg/spongepowered/asm/mixin/injection/Constant$Condition;", "GREATER_THAN_OR_EQUAL_TO_ZERO");
                    av3.visitEnd();
                }
                av2.visitEnd();
            }
            av1.visitEnd();
        }
        {
            final AnnotationVisitor av1 = av0.visitArray("slice");
            {
                final AnnotationVisitor av2 = av1.visitAnnotation(null, "Lorg/spongepowered/asm/mixin/injection/Slice;");
                {
                    final AnnotationVisitor av3 = av2.visitAnnotation("from", "Lorg/spongepowered/asm/mixin/injection/At;");
                    av3.visit("value", "INVOKE:FIRST");
                    av3.visit("target", "Lnet/minecraft/util/math/BlockPos;getY()I");
                    av3.visitEnd();
                }
                {
                    final AnnotationVisitor av3 = av2.visitAnnotation("to", "Lorg/spongepowered/asm/mixin/injection/At;");
                    av3.visit("value", "INVOKE:FIRST");
                    av3.visit("target", "Lgit/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/world/PluginChunkCache$Hooks;useNeighborBrightness(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z");
                    av3.visit("remap", false);
                    av3.visitEnd();
                }
                av2.visitEnd();
            }
            av1.visitEnd();
        }
        av0.visitEnd();
        return true;
    }
}
