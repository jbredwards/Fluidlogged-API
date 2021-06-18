package git.jbredwards.fluidlogged_api.asm.plugin;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * THIS USES ASM INSTEAD OF REPLACING THE OBJECT BECAUSE OPTIFINE COMPAT
 *
 * list of changes:
 * changes the textures to the fluidlogged ones
 * removes 0.001 offset on each face
 * colors the bottom of water blocks
 * fixes the fluid corners
 * @author jbred
 *
 */
public class BlockFluidRendererPlugin extends AbstractPlugin
{
    //shouldn't be called outside this mod
    public static float[][] CORNER_LOCAL_VAR = new float[2][2];

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
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //prevents crashes down the line
        if(insn.getPrevious() == null) return false;
        if(insn.getNext() == null) return true;

        //removes the buggy 0.001 float offset
        else if(insn.getOpcode() == LDC && ((LdcInsnNode)insn).cst.equals(0.001f)) {
            instructions.insert(insn, new InsnNode(FCONST_0));
            instructions.remove(insn);
        }
        //removes underwater surface misalignment
        else if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_176364_g" : "shouldRenderSides", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z")) {
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
        else if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_181666_a" : "color", "(FFFF)Lnet/minecraft/client/renderer/BufferBuilder;")) {
            final AbstractInsnNode param1 = ASMUtils.getPrevious(insn, 4);
            final AbstractInsnNode param2 = ASMUtils.getPrevious(insn, 3);
            final AbstractInsnNode param3 = ASMUtils.getPrevious(insn, 2);
            //checker exists cause optifine already fixes this issue
            if(param1 instanceof LdcInsnNode && param2 instanceof LdcInsnNode && param3 instanceof LdcInsnNode) {
                instructions.insert(param1, new InsnNode(FMUL));
                instructions.insert(param1, new VarInsnNode(FLOAD, 9));
                instructions.insert(param2, new InsnNode(FMUL));
                instructions.insert(param2, new VarInsnNode(FLOAD, 10));
                instructions.insert(param3, new InsnNode(FMUL));
                instructions.insert(param3, new VarInsnNode(FLOAD, 11));
            }
        }
        //stores the new corners local var
        else if(insn.getOpcode() == FSTORE && (((VarInsnNode)insn).var == 19 || ((VarInsnNode)insn).var == 20)) {
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //IBlockState param
            list.add(new VarInsnNode(ALOAD, 2));
            //IBlockAccess param
            list.add(new VarInsnNode(ALOAD, 1));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 3));
            //method
            list.add(method("storeCorners", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)V"));

            //new
            instructions.insert(insn, list);
        }
        //replaces the old corner code
        else if(insn.getOpcode() == INVOKESPECIAL && ASMUtils.checkMethod(insn, obfuscated ? "func_178269_a" : "getFluidHeight", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/material/Material;)F")) {
            instructions.insert(insn, method("getFluidHeight", "(Lnet/minecraft/client/renderer/BlockFluidRenderer;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/material/Material;Lnet/minecraft/util/math/BlockPos;)F"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 3));
            instructions.remove(insn);
        }

        //===================================================================================================
        //ALIGN WITH FLUIDLOGGED OFFSET (the fluidlogged offset was implemented on purpose to fix z-fighting)
        //===================================================================================================

        //vanilla y, line[150-153]
        else if(insn.getOpcode() == DLOAD && ((VarInsnNode)insn).var == 27 && insn.getNext().getOpcode() == DLOAD && ((VarInsnNode)insn.getNext()).var == 29) {
            instructions.insert(insn, new InsnNode(DADD));
            instructions.insert(insn, new LdcInsnNode(new Double("0.002")));
        }
        //optifine y, line[180-183]
        else if(insn.getOpcode() == DLOAD && ((VarInsnNode)insn).var == 28 && insn.getNext().getOpcode() == DLOAD && ((VarInsnNode)insn.getNext()).var == 30) {
            instructions.insert(insn, new InsnNode(DADD));
            instructions.insert(insn, new LdcInsnNode(new Double("0.002")));
        }
        //vanilla x, line[254-258][261-264]
        else if(insn.getPrevious().getOpcode() == ALOAD && ((VarInsnNode)insn.getPrevious()).var == 4 && insn.getOpcode() == DLOAD && (((VarInsnNode)insn).var == 39 || ((VarInsnNode)insn).var == 43)) {
            instructions.insert(insn, method("fixTextureFightingX", "(DILnet/minecraft/util/math/BlockPos;)D"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 3));
            instructions.insert(insn, new VarInsnNode(ILOAD, 32));
        }
        //optifine x, line[331-334][339-342]
        else if(insn.getPrevious().getOpcode() == ALOAD && ((VarInsnNode)insn.getPrevious()).var == 4 && insn.getOpcode() == DLOAD && (((VarInsnNode)insn).var == 49 || ((VarInsnNode)insn).var == 51)) {
            instructions.insert(insn, method("fixTextureFightingX", "(DILnet/minecraft/util/math/BlockPos;)D"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 3));
            instructions.insert(insn, new VarInsnNode(ILOAD, 33));
        }
        //vanilla z, line[254-258][261-264]
        else if(insn.getNext().getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn.getNext(), obfuscated ? "func_181662_b" : "pos", "(DDD)Lnet/minecraft/client/renderer/BufferBuilder;") && insn.getOpcode() == DLOAD && (((VarInsnNode)insn).var == 41 || ((VarInsnNode)insn).var == 45)) {
            instructions.insert(insn, method("fixTextureFightingZ", "(DILnet/minecraft/util/math/BlockPos;)D"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 3));
            instructions.insert(insn, new VarInsnNode(ILOAD, 32));
        }
        //optifine z, line[331-334][339-342]
        else if(insn.getNext().getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn.getNext(), obfuscated ? "func_181662_b" : "pos", "(DDD)Lnet/minecraft/client/renderer/BufferBuilder;") && insn.getOpcode() == DLOAD && (((VarInsnNode)insn).var == 53 || ((VarInsnNode)insn).var == 55)) {
            instructions.insert(insn, method("fixTextureFightingZ", "(DILnet/minecraft/util/math/BlockPos;)D"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 3));
            instructions.insert(insn, new VarInsnNode(ILOAD, 33));
        }

        return false;
    }
}
