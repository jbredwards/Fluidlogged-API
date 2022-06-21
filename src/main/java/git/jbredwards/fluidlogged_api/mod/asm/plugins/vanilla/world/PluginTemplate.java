package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * structures can load saved FluidStates
 * @author jbred
 *
 */
public final class PluginTemplate implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals("<init>")) return 1;
        else if(method.name.equals(obfuscated ? "func_186254_a" : "takeBlocksFromWorld")) return 2;
        else if(checkMethod(method, obfuscated ? "func_189960_a" : "addBlocksToWorld", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/gen/structure/template/ITemplateProcessor;Lnet/minecraft/world/gen/structure/template/PlacementSettings;I)V"))
            return 3;
        else if(method.name.equals(obfuscated ? "func_189552_a" : "writeToNBT")) return 4;
        else if(method.name.equals(obfuscated ? "func_186256_b" : "read")) return 5;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //initialize fluidStates list
        if(index == 1) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new TypeInsnNode(NEW, "java/util/ArrayList"));
            list.add(new InsnNode(DUP));
            list.add(new MethodInsnNode(INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false));
            list.add(new FieldInsnNode(PUTFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
            instructions.insert(insn, list);
            return true;
        }
        else if(index == 2) {
            //clear existing FluidState list before storing new
            if(checkMethod(insn.getPrevious(), obfuscated ? "func_177982_a" : "add", "(III)Lnet/minecraft/util/math/BlockPos;")) {
                instructions.insert(insn, new MethodInsnNode(INVOKEINTERFACE, "java/util/List", "clear", "()V", true));
                instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
                instructions.insert(insn, new VarInsnNode(ALOAD, 0));
                return false;
            }
            //gather FluidStates
            else if(checkMethod(insn.getPrevious(), obfuscated ? "func_175625_s" : "getTileEntity")) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, 13));
                list.add(new VarInsnNode(ALOAD, 14));
                list.add(new VarInsnNode(ALOAD, 5));
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
                list.add(genMethodNode("addFluidState", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;Ljava/util/List;)V"));
                instructions.insert(insn, list);
                return true;
            }
        }
        else if(index == 3) {
            //keep old fluids here
            if(checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "keepOldFluidStates", "Z"));
                instructions.insertBefore(insn, genMethodNode("keepOldFlag", "(IZ)I"));
                return false;
            }
            //place stored FluidStates
            else if(insn.getOpcode() == RETURN) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", obfuscated ? "field_186272_c" : "size", "Lnet/minecraft/util/math/BlockPos;"));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 4));
                instructions.insertBefore(insn, new VarInsnNode(ILOAD, 5));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
                instructions.insertBefore(insn, genMethodNode("addFluidsToWorld", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/gen/structure/template/PlacementSettings;ILjava/util/List;)V"));
                return true;
            }
        }
        //write FluidStates
        if(index == 4) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "keepOldFluidStates", "Z"));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
            list.add(genMethodNode("writeTemplate", "(Lnet/minecraft/nbt/NBTTagCompound;ZLjava/util/List;)V"));
            return true;
        }
        //read FluidStates
        if(index == 5) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
            list.add(genMethodNode("readTemplate", "(Lnet/minecraft/gen/structure/template/Template;Lnet/minecraft/nbt/NBTTagCompound;Ljava/util/List;)V"));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "keepOldFluidStates", "Z", null, null));
        classNode.fields.add(new FieldNode(ACC_PUBLIC | ACC_FINAL, "fluidStates", "Ljava/util/List;", "Ljava/util/List<Lorg/apache/commons/lang3/tuple/Pair<Lnet/minecraft/util/math/BlockPos;Lgit/jbredwards/fluidlogged_api/api/util/FluidState;>;>;", null));
        return true;
    }
}
