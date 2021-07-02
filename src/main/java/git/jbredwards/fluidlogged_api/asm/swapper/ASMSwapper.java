package git.jbredwards.fluidlogged_api.asm.swapper;

import com.google.common.collect.ImmutableMap;
import net.minecraft.launchwrapper.IClassTransformer;
import org.objectweb.asm.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;
import java.util.Optional;

/**
 * swaps a class's parent class for something else
 * @author jbred
 *
 */
@SuppressWarnings("unused")
public class ASMSwapper implements IClassTransformer
{
    @Nonnull public Map<String, String> SWAP = new ImmutableMap.Builder<String, String>()
            .put("net/minecraft/block/BlockLiquid", "git/jbredwards/fluidlogged_api/asm/swapper/BlockLiquidBase")
            //removes the smoothwater mod overrides and replaces them with the integrated ones
            .put("pl/asie/mage/core/water/BlockLiquidForged", "git/jbredwards/fluidlogged_api/asm/swapper/BlockLiquidBase")
            .put("pl/asie/smoothwater/BlockLiquidForged", "git/jbredwards/fluidlogged_api/asm/swapper/BlockLiquidBase")
            //sledgehammer mod compat
            .put("io/github/lxgaming/sledgehammer/mixin/core/block/BlockDynamicLiquidMixin", "git/jbredwards/fluidlogged_api/asm/swapper/BlockLiquidBase")
            .build();

    @Override
    public byte[] transform(String name, String transformedName, byte[] basicClass) {
        if(basicClass == null) return null;
        final ClassReader reader = new ClassReader(basicClass);
        final String oldName = Optional.ofNullable(reader.getSuperName()).orElse("NULL");
        final @Nullable String newName = SWAP.get(oldName);
        return newName == null ? basicClass : swap(reader, basicClass, oldName, newName);
    }

    //does the swap
    public byte[] swap(@Nonnull ClassReader reader, byte[] basicClass, @Nonnull String oldName, @Nonnull String newName) {
        final ClassWriter writer = new ClassWriter(0);
        //does not transform the new super class itself
        if(newName.equals(reader.getClassName())) return basicClass;
        //returns the transformed class
        reader.accept(new SwapperHandler(writer, oldName, newName), 0);
        return writer.toByteArray();
    }

    //handles the swap
    public static class SwapperHandler extends ClassVisitor
    {
        @Nonnull public String oldName;
        @Nonnull public String newName;

        public SwapperHandler(@Nonnull ClassWriter writer, @Nonnull String oldName, @Nonnull String newName) {
            super(Opcodes.ASM5, writer);
            this.oldName = oldName;
            this.newName = newName;
        }

        @Override
        public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
            super.visit(version, access, name, signature, newName, interfaces);
        }

        @Override
        public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
            final MethodVisitor old = super.visitMethod(access, name, desc, signature, exceptions);
            return "<init>".equals(name) ? new MethodVisitor(Opcodes.ASM5, old) {
                @Override
                public void visitMethodInsn(int opcode, String owner, String name, String desc, boolean itf) {
                    if("<init>".equals(name) && oldName.equals(owner)) {
                        super.visitMethodInsn(opcode, newName, name, desc, itf);
                    } else super.visitMethodInsn(opcode, owner, name, desc, itf);
                }
            } : old;
        }
    }
}
