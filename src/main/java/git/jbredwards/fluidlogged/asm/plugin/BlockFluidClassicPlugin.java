package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;

/**
 * allows fluidlogged parent blocks to connect with non-fluidlogged fluids (if they are of the same type)
 * @author jbred
 *
 */
public final class BlockFluidClassicPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "getQuantaValue";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == ICONST_M1) {
            //replaces line 94
            final InsnList list = new InsnList();
            //fluid block variable
            list.add(new VarInsnNode(ALOAD, 0));
            //iblockstate variable
            list.add(new VarInsnNode(ALOAD, 3));
            //quantaPerBlock variable
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "quantaPerBlock", "I"));
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/ASMHooks", "getQuantaValue", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/block/state/IBlockState;I)I", false));

            instructions.insert(insn, list);
            instructions.remove(insn);

            return true;
        }

        return false;
    }
}
