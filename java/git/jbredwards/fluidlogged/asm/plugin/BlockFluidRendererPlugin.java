package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.ASMUtils;
import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.util.FluidloggedConstants;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;

/**
 * THIS USES ASM INSTEAD OF REPLACING THE OBJECT BECAUSE OPTIFINE COMPAT
 *
 * list of changes:
 * changes the textures to the fluidlogged ones
 * removes 0.001 offset on each face
 * colors the bottom of water blocks
 * @author jbred
 *
 */
public final class BlockFluidRendererPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_178270_a" : "renderFluid";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/client/renderer/BufferBuilder;)Z";
    }

    @SuppressWarnings("UnnecessaryBoxing")
    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //removes the buggy 0.001 offset
        if(insn.getOpcode() == LDC && insn instanceof LdcInsnNode && ((LdcInsnNode)insn).cst.equals(0.001F)) {
            instructions.insert(insn, new InsnNode(FCONST_0));
            instructions.remove(insn);
        }
        //removes underwater surface misalignment
        if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_176364_g" : "shouldRenderSides", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z")) {
            final AbstractInsnNode pos1 = ASMUtils.getNext(insn, 11);
            final AbstractInsnNode pos2 = ASMUtils.getNext(pos1, 28);
            final AbstractInsnNode pos3 = ASMUtils.getNext(pos2, 28);
            final AbstractInsnNode pos4 = ASMUtils.getNext(pos3, 28);

            instructions.insert(pos1, new InsnNode(DSUB));
            instructions.insert(pos1, new LdcInsnNode(new Double(0.001)));
            instructions.insert(pos2, new InsnNode(DSUB));
            instructions.insert(pos2, new LdcInsnNode(new Double(0.001)));
            instructions.insert(pos3, new InsnNode(DSUB));
            instructions.insert(pos3, new LdcInsnNode(new Double(0.001)));
            instructions.insert(pos4, new InsnNode(DSUB));
            instructions.insert(pos4, new LdcInsnNode(new Double(0.001)));
        }
        //colors the bottom of the water block
        if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, null, "(FFFF)Lnet/minecraft/client/renderer/BufferBuilder;")) {
            final AbstractInsnNode param1 = ASMUtils.getPrevious(insn, 4);
            final AbstractInsnNode param2 = ASMUtils.getPrevious(insn, 3);
            final AbstractInsnNode param3 = ASMUtils.getPrevious(insn, 2);
            //checker exists cause optifine already fixes this issue through asm (or something similar)
            if(param1 instanceof LdcInsnNode && param2 instanceof LdcInsnNode && param3 instanceof LdcInsnNode) {
                instructions.insert(param1, new InsnNode(FMUL));
                instructions.insert(param1, new VarInsnNode(FLOAD, 9));
                instructions.insert(param2, new InsnNode(FMUL));
                instructions.insert(param2, new VarInsnNode(FLOAD, 10));
                instructions.insert(param3, new InsnNode(FMUL));
                instructions.insert(param3, new VarInsnNode(FLOAD, 11));
            }
        }

        return !method.name.equals(getMethodName(obfuscated));
    }
}
