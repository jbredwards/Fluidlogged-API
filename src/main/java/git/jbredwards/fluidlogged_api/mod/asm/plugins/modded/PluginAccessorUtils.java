package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * builds the internal AccessorUtils functions
 * @author jbred
 *
 */
public final class PluginAccessorUtils implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        switch(method.name) {
            case "getCanFluidFlow": return 1;
            case "setCanFluidFlow": return 2;
            case "canSustainPlant": return 3;
            case "getFlowDecay": return 4;
            case "hasVerticalFlow": return 5;
            case "getOptimalFlowDirections": return 6;
            case "getLargerQuanta": return 7;
            case "flowIntoBlock": return 8;
            case "getDefaultFluidState": return 10;
            case "setDefaultFluidState": return 11;
            case "setKeepOldFluidStates": return 12;
            default: return 0;
        }
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(index == 1 && insn.getOpcode() == ACONST_NULL) {
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraft/block/Block", "canFluidFlow", "Ljava/lang/Boolean;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 1);
            return true;
        }
        else if(index == 2 && insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new FieldInsnNode(PUTFIELD, "net/minecraft/block/Block", "canFluidFlow", "Ljava/lang/Boolean;"));
            setMaxLocals(method, 2);
            return true;
        }
        else if(index == 3 && insn.getOpcode() == ICONST_0) {
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/block/BlockBush", obfuscated ? "func_185514_i" : "canSustainBush", "(Lnet/minecraft/block/state/IBlockState;)Z", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 2);
            return true;
        }
        else if(index == 4 && insn.getOpcode() == ICONST_0) {
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidBase", "getFlowDecay_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 3);
            return true;
        }
        else if(index == 5 && insn.getOpcode() == ICONST_0) {
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidBase", "hasVerticalFlow_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 3);
            return true;
        }
        else if(index == 6 && insn.getOpcode() == ACONST_NULL) {
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "getOptimalFlowDirections_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)[Z", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 3);
            return true;
        }
        else if(index == 7 && insn.getOpcode() == ICONST_0) {
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "getLargerQuanta_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", false));
            instructions.insert(insn, new VarInsnNode(ILOAD, 3));
            instructions.insert(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 4);
            return true;
        }
        else if(index == 8 && insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, new VarInsnNode(ILOAD, 3));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "flowIntoBlock_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", false));
            setMaxLocals(method, 4);
            return true;
        }
        else if(index == 10 && insn.getOpcode() == ACONST_NULL) {
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/Fluid", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 1);
            return true;
        }
        else if(index == 11 && insn.getOpcode() == ACONST_NULL) {
            instructions.insert(insn, new FieldInsnNode(PUTFIELD, "net/minecraftforge/fluids/Fluid", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            instructions.insert(insn, new InsnNode(DUP_X1));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            setMaxLocals(method, 3);
            return true;
        }
        else if(index == 12 && insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ILOAD, 1));
            instructions.insertBefore(insn, new FieldInsnNode(PUTFIELD, "net/minecraft/world/gen/structure/template/Template", "keepOldFluidStates", "Z"));
            setMaxLocals(method, 2);
            return true;
        }

        return false;
    }
}
