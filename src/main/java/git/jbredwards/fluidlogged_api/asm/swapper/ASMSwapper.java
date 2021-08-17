package git.jbredwards.fluidlogged_api.asm.swapper;

import com.google.common.collect.ImmutableMap;
import mcp.MethodsReturnNonnullByDefault;
import net.minecraft.launchwrapper.IClassTransformer;
import org.objectweb.asm.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.ParametersAreNonnullByDefault;
import java.util.Map;

/**
 * swaps a class's parent class for something else
 * @author jbred
 *
 */
@MethodsReturnNonnullByDefault
@ParametersAreNonnullByDefault
@SuppressWarnings("unused")
public class ASMSwapper implements IClassTransformer
{
    @Nonnull public static Map<String, String> SWAP = new ImmutableMap.Builder<String, String>()
            .put("net/minecraft/block/BlockLiquid", "git/jbredwards/fluidlogged_api/asm/swapper/BlockLiquidBase")
            //removes the smoothwater mod overrides and replaces them with the integrated ones
            .put("pl/asie/mage/core/water/BlockLiquidForged", "git/jbredwards/fluidlogged_api/asm/swapper/BlockLiquidBase")
            .put("pl/asie/smoothwater/BlockLiquidForged", "git/jbredwards/fluidlogged_api/asm/swapper/BlockLiquidBase")
            .build();

    @Nullable
    @Override
    public byte[] transform(@Nullable String name, @Nullable String transformedName, @Nullable byte[] basicClass) {
        if(basicClass == null) return null;
        final ClassReader reader = new ClassReader(basicClass);

        final @Nullable String oldName = reader.getSuperName();
        if(oldName == null) return basicClass;

        final @Nullable String newName = SWAP.get(oldName);
        return newName == null ? basicClass : swap(reader, basicClass, oldName, newName);
    }

    //does the swap
    public byte[] swap(ClassReader reader, byte[] basicClass, String oldName, String newName) {
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

        public SwapperHandler(ClassWriter writer, String oldName, String newName) {
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
