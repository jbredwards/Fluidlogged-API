package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * modded fluids work properly with the mod & prevent startup crash
 * @author jbred
 *
 */
public final class PluginBlockFluidBase implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        //constructor
        if(checkMethod(method, "<init>", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/material/Material;Lnet/minecraft/block/material/MapColor;)V")) return 1;
        //defaultDisplacements clinit
        else if(checkMethod(method, "<clinit>", "()V")) return 2;
        //canDisplace, check fluid rather than block
        else if(method.name.equals("canDisplace")) return 3;
        //getFlowDirection
        else if(method.name.equals("getFlowDirection") || method.name.equals("getDensity")) return 4;
        //getFluid
        else if(method.name.equals("getFluid")) return 5;
        //getFilledPercentage
        else if(checkMethod(method, "getFilledPercentage", "(Lnet/minecraft.world.IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F")) return 6;
        //getStateAtViewpoint
        else if(method.name.equals("getStateAtViewpoint")) return 7;
        //default
        else return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //constructor, line 181
        if(index == 1 && checkField(insn, "defaultDisplacements", "Ljava/util/Map;")) {
            instructions.insert(insn, genMethodNode("defaultDisplacements", "(Ljava/util/Map;)Ljava/util/Map;"));
            return true;
        }
        //clinit, line 73-113
        else if(index == 2 && insn.getNext().getOpcode() == LDC) {
            //removes all default entries, as the class is now loaded before they're registered
            while(insn.getPrevious().getOpcode() != PUTSTATIC) instructions.remove(insn.getPrevious());
            return true;
        }
        //canDisplace, check FluidState rather than block
        else if(index == 3 && insn.getOpcode() == IF_ACMPNE) {
            final InsnList list = new InsnList();
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidBase", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", false));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 3));
            list.add(genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", false));
            list.add(genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "isCompatibleFluid", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraftforge/fluids/Fluid;)Z"));
            //remove old
            instructions.remove(getPrevious(insn, 2));
            instructions.insertBefore(insn, list);
            ((JumpInsnNode)insn).setOpcode(IFEQ);
            return true;
        }
        //getFlowDirection, account for FluidState
        else if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        //getFluid, line 792 (exposed to significantly boost performance, sorry forge devs but why'd you not want this exposed again?)
        else if(index == 5 && checkMethod(insn, "getFluid", "(Ljava/lang/String;)Lnet/minecraftforge/fluids/Fluid;")) {
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);
            return true;
        }
        //getFilledPercentage, fix inaccuracies with some modded fluids
        else if(index == 6 && insn.getOpcode() == FMUL) {
            instructions.insertBefore(insn, new InsnNode(FMUL));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaFraction", "F"));
            return true;
        }
        //getStateAtViewpoint, return the other block here if the player isn't within the fluid
        else if(index == 7 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("getStateAtViewpoint", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 4));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            removeFrom(instructions, insn, -2);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        //shouldSideBeRendered, check for neighboring fluids rather than neighboring blocks
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176225_a" : "shouldSideBeRendered"),
            "shouldFluidSideBeRendered", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitMaxs(5, 0);
            }
        );
        //getExtendedState, fix corner heights
        overrideMethod(classNode, method -> method.name.equals("getExtendedState"),
            "getFluidExtendedState", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;IIFFF)Lnet/minecraft/block/state/IBlockState;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraftforge/fluids/IFluidBlock", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", true);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlockFloat", "F");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaFraction", "F");
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitMethodInsn(INVOKESTATIC, "net/minecraftforge/fluids/BlockFluidBase", "getFlowDirection", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)D", false);
                generator.visitInsn(D2F);
                generator.visitMaxs(10, 0);
            }
        );
        //getFlowVector, don't flow into/from invalid sides
        overrideMethod(classNode, method -> method.name.equals("getFlowVector"),
            "getFluidFlowVector", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;II)Lnet/minecraft/util/math/Vec3d;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
                generator.visitMaxs(5, 0);
            }
        );
        //hasVerticalFlow, don't flow into/from invalid sides
        overrideMethod(classNode, method -> method.name.equals("hasVerticalFlow"),
            "hasVerticalFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraftforge/fluids/IFluidBlock", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", true);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitMaxs(4, 0);
            }
        );
        //getFogColor, remove built-in for better checks
        overrideMethod(classNode, method -> method.name.equals("getFogColor"),
            "getFluidFogColor", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/Entity;Lnet/minecraft/util/math/Vec3d;F)Lnet/minecraft/util/math/Vec3d;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(ALOAD, 5);
                generator.visitVarInsn(FLOAD, 6);
                generator.visitMaxs(6, 0);
            }
        );
        //better entity fluid collision
        addMethod(classNode, "isEntityInsideMaterial", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;",
            "isEntityInsideFluid", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(DLOAD, 5);
                generator.visitVarInsn(ALOAD, 7);
                generator.visitVarInsn(ILOAD, 8);
                generator.visitMaxs(9, 0);
            }
        );
        //better entity fluid collision
        addMethod(classNode, "isAABBInsideMaterial", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;",
            "isAABBInsideMaterial", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitMaxs(5, 0);
            }
        );
        //better entity fluid collision
        addMethod(classNode, "isAABBInsideLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/util/math/AxisAlignedBB", obfuscated ? "field_72338_b" : "minY", "D");
            generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "isWithinFluid", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;D)Z", false);
            generator.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
            generator.visitMaxs(5, 0);
        });
        //add public getter for protected method
        addMethod(classNode, "getFlowDecay_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidBase", "getFlowDecay", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", false);
            generator.visitMaxs(3, 0);
        });
        //add public getter for protected method
        addMethod(classNode, "hasVerticalFlow_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidBase", "hasVerticalFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", false);
            generator.visitMaxs(3, 0);
        });

        return true;
    }
}
