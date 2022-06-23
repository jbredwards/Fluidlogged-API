package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * modded fluids work properly with the mod
 * @author jbred
 *
 */
public final class PluginBlockFluidClassic implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("getOptimalFlowDirections"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //save state here for later use
        if(instructions.getFirst() == insn.getPrevious()) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEINTERFACE, "net/minecraft/world/IBlockAccess", obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", true));
            instructions.insertBefore(insn, new VarInsnNode(ASTORE, 5));
        }
        //ignore if fluid can't flow from the given side
        else if(checkMethod(insn, "isSourceBlock")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 5));
            list.add(new VarInsnNode(ILOAD, 3));
            list.add(genMethodNode("getOptimalFlowDirections", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));

            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid");
        //able to get quanta value from fluidlogged fluids
        overrideMethod(classNode, method -> method.name.equals("getQuantaValue"),
            "getQuantaValue", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
            }
        );
        //able to get quanta value from fluidlogged fluids
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180650_b" : "updateTick"),
            "fluidUpdateTick", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;IIZ)V", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "canCreateSources", "Z");
            }
        );
        //check for FluidStates & canFluidFlow
        overrideMethod(classNode, method -> method.name.equals("isFlowingVertically"),
            "isFluidFlowingVertically", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
            }
        );
        //check for ICompatibleFluid
        overrideMethod(classNode, method -> method.name.equals("canFlowInto"),
            "canFlowInto", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        //check for FluidStates & canFluidFlow
        overrideMethod(classNode, method -> method.name.equals("isSourceBlock"),
            "isSourceBlock", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraft/world/IBlockAccess", obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", true);
                generator.visitInsn(ACONST_NULL);
            }
        );
        //allow the place method to fluidlog blocks
        overrideMethod(classNode, method -> method.name.equals("place"),
            "place", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;ZLnet/minecraft/block/state/IBlockState;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKESPECIAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false);
            }
        );
        //allow the drain method to drain fluidlogged blocks
        overrideMethod(classNode, method -> method.name.equals("drain"),
            "drain", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ZLnet/minecraftforge/fluids/FluidStack;)Lnet/minecraftforge/fluids/FluidStack;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "stack", "Lnet/minecraftforge/fluids/FluidStack;");
            }
        );
        //allow this fluid to be fluidloggable
        addMethod(classNode, "isFluidloggableFluid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z",
            "isFluidFluidloggable", "(Lgit/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;IZ)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "canCreateSources", "Z");
            }
        );
        //allow this fluid to be fluidloggable
        addMethod(classNode, "isFluidloggableFluid", "()Z", "isFluidFluidloggable", "(Lnet/minecraft/block/Block;)Z",
                generator -> generator.visitVarInsn(ALOAD, 0));
        //add public getter for protected method
        addMethod(classNode, "getOptimalFlowDirections_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)[Z", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", "getOptimalFlowDirections", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)[Z", false);
        });
        //add public getter for protected method
        addMethod(classNode, "getLargerQuanta_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", "getLargerQuanta", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", false);
        });
        //add public getter for protected method
        addMethod(classNode, "canFlowInto_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", "canFlowInto", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", false);
        });
        //add public getter for protected method
        addMethod(classNode, "flowIntoBlock_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidClassic", "flowIntoBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", false);
        });

        return true;
    }

    //getOptimalFlowDirections, don't look up the state here each loop
    @Override
    public boolean addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) {
        method.localVariables.add(new LocalVariableNode("here", "Lnet/minecraft/block/state/IBlockState;", null, start, end, 5));
        return true;
    }
}
