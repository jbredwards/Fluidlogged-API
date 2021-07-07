package git.jbredwards.fluidlogged_api.asm.plugin.forge;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

import java.util.Iterator;

/**
 * -no longer flow into non-replaceable blocks
 * -fixes issues with vertical flowing fluids
 * -fixes issues with fluid angles
 * @author jbred
 *
 */
public class BlockFluidBasePlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //init
        if(ASMUtils.checkMethod(method, "<init>", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/material/Material;Lnet/minecraft/block/material/MapColor;)V")) {
            currentMethod = 1;
            return true;
        }
        //canDisplace
        if(ASMUtils.checkMethod(method, "canDisplace", null)) {
            currentMethod = 2;
            return true;
        }
        //shouldSideBeRendered
        if(ASMUtils.checkMethod(method, obfuscated ? "func_176225_a" : "shouldSideBeRendered", null)) {
            currentMethod = 3;
            return true;
        }
        //getExtendedState
        if(ASMUtils.checkMethod(method, "getExtendedState", null)) {
            currentMethod = 4;
            return true;
        }
        //getFlowVector
        if(ASMUtils.checkMethod(method, "getFlowVector", null)) {
            currentMethod = 5;
            return true;
        }
        //hasVerticalFlow
        if(ASMUtils.checkMethod(method, "hasVerticalFlow", null)) {
            currentMethod = 6;
            return true;
        }
        //causesDownwardCurrent
        if(ASMUtils.checkMethod(method, "causesDownwardCurrent", null)) {
            currentMethod = 7;
            return true;
        }
        //defaultDisplacements clinit
        if(ASMUtils.checkMethod(method, "<clinit>", "()V")) {
            currentMethod = 8;
            return true;
        }

        //default
        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //init, line 181
        if(currentMethod == 1 && insn.getOpcode() == GETSTATIC && ASMUtils.checkField(insn, "defaultDisplacements", "Ljava/util/Map;")) {
            instructions.insert(insn, method("defaultDisplacements", "(Ljava/util/Map;)Ljava/util/Map;"));
        }
        //canDisplace, line 284
        if(currentMethod == 2 && insn.getOpcode() == IF_ACMPNE) {
            final InsnList list = new InsnList();
            //this param
            list.add(new VarInsnNode(ALOAD, 0));
            //IBlockAccess param
            list.add(new VarInsnNode(ALOAD, 1));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 2));

            list.add(method("canDisplace", "(Lnet/minecraft/block/Block;Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/Block;"));
            instructions.insert(ASMUtils.getPrevious(insn, 2), list);

            return true;
        }
        //shouldSideBeRendered, line 474
        if(currentMethod == 3 && insn.getOpcode() == INVOKEINTERFACE && insn.getNext().getOpcode() == ALOAD && ASMUtils.checkMethod(insn, obfuscated ? "func_185904_a" : "getMaterial", null)) {
            final InsnList list = new InsnList();
            //definedFluid var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            //IBlockAccess param
            list.add(new VarInsnNode(ALOAD, 2));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 3));
            //EnumFacing param
            list.add(new VarInsnNode(ALOAD, 4));

            list.add(method("shouldSideBeRendered", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraft/block/material/Material;"));
            instructions.insert(insn, list);
            instructions.remove(insn);

            return true;
        }
        //getExtendedState
        if(currentMethod == 4) {
            if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, "getFluidHeightForRender", null)) {
                //first instance
                if(ASMUtils.getPrevious(insn, 2).getOpcode() == ICONST_1) {
                    method.maxStack += 4;

                    final InsnList list = new InsnList();
                    list.add(new InsnNode(ICONST_1));
                    list.add(new InsnNode(ICONST_1));
                    //densityDir var
                    list.add(new VarInsnNode(ALOAD, 0));
                    list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I"));
                    //quantaPerBlock var
                    list.add(new VarInsnNode(ALOAD, 0));
                    list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I"));
                    //quantaFraction var
                    list.add(new VarInsnNode(ALOAD, 0));
                    list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaFraction", "F"));
                    //quantaPerBlockFloat var
                    list.add(new VarInsnNode(ALOAD, 0));
                    list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlockFloat", "F"));
                    //method
                    list.add(method("getFluidHeightForRender", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;IIIIFF)F"));

                    instructions.insert(insn, list);

                }
                //second instance
                else {
                    final InsnList list = new InsnList();
                    //i local var
                    list.add(new VarInsnNode(ILOAD, 8));
                    //j local var
                    list.add(new VarInsnNode(ILOAD, 9));
                    //densityDir var
                    list.add(new VarInsnNode(ALOAD, 0));
                    list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I"));
                    //quantaPerBlock var
                    list.add(new VarInsnNode(ALOAD, 0));
                    list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I"));
                    //quantaFraction var
                    list.add(new VarInsnNode(ALOAD, 0));
                    list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaFraction", "F"));
                    //quantaPerBlockFloat var
                    list.add(new VarInsnNode(ALOAD, 0));
                    list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlockFloat", "F"));
                    //method
                    list.add(method("getFluidHeightForRender", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;IIIIFF)F"));

                    instructions.insert(insn, list);

                }

                instructions.remove(insn);
                return false;
            }
            //line 528
            if(insn.getOpcode() == ICONST_4) {
                final InsnList list = new InsnList();
                //quantaFraction var
                list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaFraction", "F"));
                //i local var
                list.add(new VarInsnNode(ILOAD, 8));
                //j local var
                list.add(new VarInsnNode(ILOAD, 9));

                instructions.insertBefore(insn, list);
                return false;
            }
            //line 528
            if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, "getFluidHeightAverage", "([F)F")) {
                instructions.insert(insn, method("getFluidHeightAverage", "(FII[F)F"));
                instructions.remove(insn);
                return true;
            }
        }
        //getFlowVector
        if(currentMethod == 5) {
            //line 723
            if(insn.getOpcode() == INVOKESPECIAL && ASMUtils.checkMethod(insn, "getFlowDecay", null) && insn.getPrevious().getOpcode()== ALOAD && ((VarInsnNode)insn.getPrevious()).var == 7) {
                final InsnList list = new InsnList();
                //EnumFacing local var
                list.add(new VarInsnNode(ALOAD, 6));
                //definedFluid var
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
                //quantaPerBlock var
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I"));
                //densityDir var
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I"));
                //method
                list.add(method("getFlowDecay", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraftforge/fluids/Fluid;II)I"));

                instructions.insert(insn, list);
                instructions.remove(insn);

                return false;
            }
            //line 726
            if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_76230_c" : "blocksMovement", "()Z")) {
                final InsnList list = new InsnList();
                //side local var
                list.add(new VarInsnNode(ALOAD, 6));
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

                return true;
            }
        }
        //hasVerticalFlow
        if(currentMethod == 6 && insn.getOpcode() == IF_ACMPNE) {
            ++method.maxStack;

            //adds new
            instructions.insert(ASMUtils.getPrevious(insn, 5), new VarInsnNode(ALOAD, 0));
            instructions.insert(ASMUtils.getPrevious(insn, 5), method("hasVerticalFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;ILnet/minecraftforge/fluids/BlockFluidBase;)Lnet/minecraft/block/Block;"));

            //removes old
            instructions.remove(ASMUtils.getPrevious(insn, 4));
            instructions.remove(ASMUtils.getPrevious(insn, 3));
            instructions.remove(ASMUtils.getPrevious(insn, 2));

            return true;
        }
        //causesDownwardCurrent, line 780
        if(currentMethod == 7 && insn.getOpcode() == IF_ACMPNE) {
            //this local var
            instructions.insert(ASMUtils.getPrevious(insn, 2), new VarInsnNode(ALOAD, 0));
            //definedFluid var
            instructions.insert(ASMUtils.getPrevious(insn, 2), new VarInsnNode(ALOAD, 0));
            instructions.insert(ASMUtils.getPrevious(insn, 2), new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            //method
            instructions.insert(ASMUtils.getPrevious(insn, 2), method("causesDownwardCurrent", "(Lnet/minecraft/block/Block;Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraftforge/fluids/Fluid;)Lnet/minecraft/block/Block;"));

            return true;
        }
        //clinit, line 73-113
        if(currentMethod == 8 && insn.getOpcode() == GETSTATIC && ASMUtils.checkField(insn, "defaultDisplacements", "Ljava/util/Map;")) {
            while(!(insn.getNext().getNext() instanceof LineNumberNode && ((LineNumberNode)insn.getNext().getNext()).line == 125)) {
                instructions.remove(insn.getNext());
            }

            instructions.remove(insn);
            finishedAll = true;
            return true;
        }

        return false;
    }
}
