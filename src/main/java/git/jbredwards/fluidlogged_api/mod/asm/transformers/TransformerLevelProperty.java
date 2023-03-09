package git.jbredwards.fluidlogged_api.mod.asm.transformers;

import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * change all calls from state.getValue(BlockLiquid.LEVEL) to use FluidState.of(state).getLevel(), greatly increases performance
 * @author jbred
 *
 */
@SuppressWarnings("unused")
public final class TransformerLevelProperty implements IClassTransformer, Opcodes
{
    @Nullable
    @Override
    public byte[] transform(@Nonnull String name, @Nonnull String transformedName, @Nullable byte[] basicClass) {
        if(basicClass == null) return null;
        final ClassNode classNode = new ClassNode();
        new ClassReader(basicClass).accept(classNode, 0);

        //don't apply the change to the actual FluidState class, since it must be called there to cache the value
        if("git/jbredwards/fluidlogged_api/api/util/FluidState".equals(classNode.name)) return basicClass;

        boolean wasClassTransformed = false;
        final boolean obfuscated = !FMLLaunchHandler.isDeobfuscatedEnvironment();
        for(MethodNode method : classNode.methods) {
            if(!method.name.equals(obfuscated ? "func_176201_c" : "getMetaFromState")) {
                for(AbstractInsnNode insn : method.instructions.toArray()) {
                    if(insn instanceof FieldInsnNode && insn.getNext() instanceof MethodInsnNode && ((MethodInsnNode)insn.getNext()).name.equals(obfuscated ? "func_177229_b" : "getValue")) {
                        final FieldInsnNode field = (FieldInsnNode)insn;
                        //check vanilla BlockLiquid.LEVEL
                        if(field.name.equals(obfuscated ? "field_176367_b" : "LEVEL") && field.owner.equals("net/minecraft/block/BlockLiquid")) {
                            applyToInstructions(method.instructions, insn);
                            wasClassTransformed = true;
                        }

                        //check forge BlockFluidBase.LEVEL
                        else if(field.name.equals("LEVEL") && field.owner.equals("net/minecraftforge/fluids/BlockFluidBase")) {
                            applyToInstructions(method.instructions, insn);
                            wasClassTransformed = true;
                        }
                    }
                }
            }
        }

        //return the transformed class if any transformations happened
        if(wasClassTransformed) {
            final ClassWriter writer = new ClassWriter(0);
            classNode.accept(writer);
            return writer.toByteArray();
        }

        return basicClass;
    }

    static void applyToInstructions(@Nonnull InsnList instructions, @Nonnull AbstractInsnNode insn) {
        instructions.insertBefore(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "of", "(Lnet/minecraft/block/state/IBlockState;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", false));
        instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getLevel", "()I", false));

        //don't perform ((Integer)state.getValue(BlockLiquid.LEVEL)).intValue() if possible
        boolean isJavaInteger = true;
        if(insn.getNext() != null && insn.getNext().getNext() != null && insn.getNext().getNext().getNext() instanceof MethodInsnNode) {
            final MethodInsnNode method = (MethodInsnNode)insn.getNext().getNext().getNext();
            if(method.name.equals("intValue") && method.owner.equals("java/lang/Integer")) {
                instructions.remove(insn.getNext());
                instructions.remove(insn.getNext());
                isJavaInteger = false;
            }
        }

        if(isJavaInteger) {
            instructions.insertBefore(insn, new MethodInsnNode(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false));
        }

        instructions.remove(insn.getNext());
        instructions.remove(insn);
    }
}
