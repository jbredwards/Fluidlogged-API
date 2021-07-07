package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes the following issues:
 * -vanilla fluids no longer do block mixing when they shouldn't
 * -vanilla fluids no longer flow from sides they shouldn't
 * -vanilla fluids no longer flow into non-replaceable fluidlogged blocks
 * @author jbred
 *
 */
public class BlockDynamicLiquidPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_180650_b" : "updateTick";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //line 47
        if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_176371_a" : "checkAdjacentBlock", null)) {
            //adds new
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "checkBlockHorizontal", "(Lnet/minecraft/block/BlockDynamicLiquid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ILnet/minecraft/util/EnumFacing;)I", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 10));
            //removes old
            instructions.remove(insn);
        }
        //line 57
        else if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_189542_i" : "getDepth", null)) {
            //adds new
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "getDepth", "(Lnet/minecraft/block/BlockDynamicLiquid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I", false));
            instructions.insert(insn, new FieldInsnNode(GETSTATIC, "net/minecraft/util/EnumFacing", "UP", "Lnet/minecraft/util/EnumFacing;"));
            //removes old
            instructions.remove(ASMUtils.getPrevious(insn, 1));
            instructions.remove(ASMUtils.getPrevious(insn, 0));
        }
        //line 118
        else if(insn.getOpcode() == INVOKESPECIAL && ASMUtils.checkMethod(insn, obfuscated ? "func_176373_h" : "canFlowInto", null)) {
            //adds new
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "canFlowDown", "(Lnet/minecraft/block/BlockDynamicLiquid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/state/IBlockState;)Z", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 3));
            //removes old
            instructions.remove(insn);
        }
        //line 120
        else if(insn.getOpcode() == GETSTATIC && ASMUtils.getPrevious(insn, 2).getOpcode() == INVOKEVIRTUAL && ASMUtils.checkField(insn, obfuscated ? "field_151586_h" : "WATER", null)) {
            //adds new
            instructions.insertBefore(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "checkForMixing", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/material/Material;", false));
            //removes old
            instructions.remove(ASMUtils.getPrevious(insn, 4));
            instructions.remove(ASMUtils.getPrevious(insn, 3));
            instructions.remove(ASMUtils.getPrevious(insn, 2));
        }
        //line 138
        else if(insn.getOpcode() == INVOKESPECIAL && ASMUtils.checkMethod(insn, obfuscated ? "func_176376_e" : "getPossibleFlowDirections", null)) {
            //adds new
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "flowInto", "(Lnet/minecraft/block/BlockDynamicLiquid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;II)Ljava/util/Set;", false));
            instructions.insert(insn, new VarInsnNode(ILOAD, 6));
            instructions.insert(insn, new VarInsnNode(ILOAD, 5));
            //removes old
            instructions.remove(insn);

            //last transformation, so return true
            return true;
        }

        return false;
    }
}