package git.jbredwards.fluidlogged_api.asm.plugin;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * vanilla fluids no longer do block mixing when they shouldn't
 * @author jbred
 *
 */
public final class BlockLiquidPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //causesDownwardCurrent
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176212_b" : "causesDownwardCurrent", null)) {
            currentMethod = 1;
            return true;
        }
        //getFlow
        if(ASMUtils.checkMethod(method, obfuscated ? "func_189543_a" : "getFlow", null)) {
            currentMethod = 2;
            return true;
        }
        //checkForMixing
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176365_e" : "checkForMixing", null)) {
            currentMethod = 3;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //causesDownwardCurrent, line 113
        if(currentMethod == 1 && insn.getOpcode() == ALOAD && ((VarInsnNode)insn).var == 6) {
            instructions.insert(insn, method("causesDownwardCurrent", "(Lnet/minecraft/block/material/Material;Lnet/minecraft/block/Block;)Lnet/minecraft/block/material/Material;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 5));

            return true;
        }
        //getFlow
        if(currentMethod == 2) {
            //line 181
            if(insn.getOpcode() == ISTORE && ((VarInsnNode)insn).var == 14) {
                final InsnList list = new InsnList();
                //IBlockAccess param
                list.add(new VarInsnNode(ALOAD, 1));
                //BlockPos local var
                list.add(new VarInsnNode(ALOAD, 11));
                //EnumFacing local var
                list.add(new VarInsnNode(ALOAD, 13));
                //method
                list.add(method("getRenderedDepth", "(Lnet/minecraft/block/BlockLiquid;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I"));

                instructions.insert(ASMUtils.getPrevious(insn, 2), list);
                instructions.remove(insn.getPrevious());

                return false;
            }
            //line 185
            if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_76230_c" : "blocksMovement", "()Z")) {
                final InsnList list = new InsnList();
                //IBlockAccess param
                list.add(new VarInsnNode(ALOAD, 1));
                //BlockPos local var
                list.add(new VarInsnNode(ALOAD, 11));
                //EnumFacing local var
                list.add(new VarInsnNode(ALOAD, 13));
                //this param
                list.add(new VarInsnNode(ALOAD, 0));
                //method
                list.add(method("getFlow", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraft/block/BlockLiquid;)Z"));
                instructions.insert(insn, list);

                //removes old
                instructions.remove(ASMUtils.getPrevious(insn, 1));
                instructions.remove(ASMUtils.getPrevious(insn, 0));

                return true;
            }
        }
        //checkForMixing, line 308
        if(currentMethod == 3 && insn.getOpcode() == GETSTATIC && ASMUtils.checkField(insn, obfuscated ? "field_151586_h" : "WATER", null)) {
            //adds the new condition
            instructions.insertBefore(insn, method("checkForMixing", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraft/block/material/Material;"));

            //removes the old condition
            instructions.remove(ASMUtils.getPrevious(insn, 4));
            instructions.remove(ASMUtils.getPrevious(insn, 3));
            instructions.remove(ASMUtils.getPrevious(insn, 2));

            return true;
        }

        return false;
    }
}