package git.jbredwards.fluidlogged_api.asm.plugin.compat;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * the betweenlands mod for some reason feels the need to override every BlockFluidClassic method?!
 * this undoes all of that
 * @author jbred
 *
 */
public class BetweenlandsPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //canDisplace
        if(ASMUtils.checkMethod(method, "canDisplace", null)) {
            currentMethod = 1;
            return true;
        }
        //displaceIfPossible
        if(ASMUtils.checkMethod(method, "displaceIfPossible", null)) {
            currentMethod = 1;
            return true;
        }
        //getFlowVector
        if(ASMUtils.checkMethod(method, "getFlowVector", null)) {
            currentMethod = 2;
            return true;
        }
        //getQuantaValue
        if(ASMUtils.checkMethod(method, "getQuantaValue", null)) {
            currentMethod = 3;
            return true;
        }
        //canFlowInto
        if(ASMUtils.checkMethod(method, "canFlowInto", null)) {
            currentMethod = 4;
            return true;
        }
        //updateTick
        if(ASMUtils.checkMethod(method, obfuscated ? "func_180650_b" : "updateTick", null)) {
            currentMethod = 5;
            return true;
        }

        return false;
    }

    public boolean condition(AbstractInsnNode insn) {
        return insn.getOpcode() == INSTANCEOF && ((TypeInsnNode)insn).desc.equals("BlockSwampWater");
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //prevents crashes
        if(insn.getNext() == null) return false;

        //[canDisplace, line 84][displaceIfPossible, line 119]
        if(currentMethod == 1 && condition(insn)) {
            final InsnList list = new InsnList();
            //this param
            list.add(new VarInsnNode(ALOAD, 0));
            //definedFluid var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            //IBlockAccess param
            list.add(new VarInsnNode(ALOAD, 1));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 2));
            //checks if replaceable
            list.add(new InsnNode(ICONST_1));
            //method
            list.add(method("modCompat", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/Block;Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Z)Lnet/minecraft/block/Block;"));

            instructions.insert(insn, list);
            return true;
        }
        //getFlowVector
        if(currentMethod == 2) {
            //line 186
            if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_76230_c" : "blocksMovement", "()Z")) {
                final InsnList list = new InsnList();
                //convert side local var to facing
                list.add(new VarInsnNode(ILOAD, 5));
                list.add(method("betweenlandsFacing", "(I)Lnet/minecraft/util/EnumFacing;"));
                //definedFluid var
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
                //method
                list.add(method("getFlowVector", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraftforge/fluids/Fluid;)Z"));
                instructions.insert(insn, list);

                //removes old
                instructions.remove(ASMUtils.getPrevious(insn, 2));
                instructions.remove(ASMUtils.getPrevious(insn, 1));
                instructions.remove(ASMUtils.getPrevious(insn, 0));

                return false;
            }
            //line 203
            if(condition(insn)) {
                final InsnList list = new InsnList();
                //this param
                list.add(new VarInsnNode(ALOAD, 0));
                //definedFluid var
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
                //IBlockAccess param
                list.add(new VarInsnNode(ALOAD, 1));
                //BlockPos param
                list.add(new VarInsnNode(ALOAD, 2));
                //method
                list.add(method("modCompat", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/Block;Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/Block;"));

                instructions.insert(insn, list);
                return true;
            }
        }
        //getQuantaValue, line 246
        if(currentMethod == 3 && insn.getNext().getOpcode() == IFNE && condition(insn)) {
            final InsnList list = new InsnList();
            //this param
            list.add(new VarInsnNode(ALOAD, 0));
            //definedFluid var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            //IBlockAccess param
            list.add(new VarInsnNode(ALOAD, 1));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 2));
            //method
            list.add(method("modCompat", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/Block;Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/Block;"));

            instructions.insertBefore(insn, list);
            instructions.remove(insn);
            return true;
        }
        //canFlowInto, line 266
        if(currentMethod == 4 && condition(insn)) {
            final InsnList list = new InsnList();
            //this param
            list.add(new VarInsnNode(ALOAD, 0));
            //definedFluid var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            //IBlockAccess param
            list.add(new VarInsnNode(ALOAD, 1));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 2));
            //doesn't check if replaceable
            list.add(new InsnNode(ICONST_0));
            //method
            list.add(method("modCompat", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/Block;Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Z)Lnet/minecraft/block/Block;"));

            instructions.insert(insn, list);
            return true;
        }
        //updateTick
        if(currentMethod == 4) {

        }

        return false;
    }
}
