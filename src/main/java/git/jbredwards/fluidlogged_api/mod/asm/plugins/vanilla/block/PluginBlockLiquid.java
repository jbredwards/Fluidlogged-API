package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * makes liquids fluidloggable
 * @author jbred
 *
 */
public final class PluginBlockLiquid implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_149645_b" : "getRenderType")) return 1;
        else if(method.name.equals("getFogColor")) return 2;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //change the render type to a model
        if(index == 1 && checkField(insn, "LIQUID")) ((FieldInsnNode)insn).name = "MODEL";
        //fix fog color to work with new fluid collision
        else if(index == 2) {
            //add better check
            if(checkMethod(insn, obfuscated ? "func_76224_d" : "isLiquid")) {
                instructions.insert(insn, genMethodNode("getLiquidFogColor", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Z"));
                instructions.insert(insn, new VarInsnNode(ALOAD, 7));
                instructions.insert(insn, new VarInsnNode(ALOAD, 2));
                instructions.insert(insn, new VarInsnNode(ALOAD, 1));
                removeFrom(instructions, insn, -1);
            }
            //remove unused if statement
            else if(insn.getOpcode() == INSTANCEOF)
                ((TypeInsnNode)insn).desc = "java/lang/String";
            //remove other unused if statement
            else if(insn.getOpcode() == IFLE) {
                removeFrom(instructions, insn.getPrevious(), -4);
                instructions.insertBefore(insn, new InsnNode(ICONST_1));
                ((JumpInsnNode)insn).setOpcode(IFEQ);
            }
            //return original color if entity isn't within fluid
            else if(checkMethod(insn, "getFogColor")) {
                instructions.insert(insn, new VarInsnNode(ALOAD, 5));
                removeFrom(instructions, insn, -20);
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid");
        //use forge unlisted fluid props
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180661_e" : "createBlockState"),
            "createLiquidBlockState", "(Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/BlockStateContainer;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMaxs(1, 0);
            }
        );
        //better side rendering
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176225_a" : "shouldSideBeRendered"),
            "shouldFluidSideBeRendered", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitInsn(ICONST_M1);
                generator.visitMaxs(5, 0);
            }
        );
        //fix canFluidFlow-related vector bugs
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_189543_a" : "getFlow"),
            "getFlow", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/util/math/Vec3d;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitMaxs(4, 0);
            }
        );
        //stone & obsidian only form while directly connected to lava/water
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176365_e" : "checkForMixing"),
            "checkForMixing", "(Lnet/minecraft/block/BlockLiquid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitMaxs(4, 0);
            }
        );
        //stone & obsidian only form while directly connected to lava/water
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_190973_f" : "getBlockLiquidHeight"),
            "getBlockLiquidHeight", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitMaxs(3, 0);
            }
        );
        //fix corner heights
        addMethod(classNode, "getExtendedState", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;",
            "getFluidExtendedState", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;IIFFF)Lnet/minecraft/block/state/IBlockState;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraftforge/fluids/IFluidBlock", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", true);
                generator.visitInsn(ICONST_M1);
                generator.visitLdcInsn(8);
                generator.visitLdcInsn(8f);
                generator.visitLdcInsn(8f/9);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "getSlopeAngle", "(Lnet/minecraft/block/BlockLiquid;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)D", false);
                generator.visitInsn(D2F);
                generator.visitMaxs(11, 0);
            }
        );
        //getStateAtViewpoint
        addMethod(classNode, "getStateAtViewpoint", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;",
            "getStateAtViewpoint", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitMaxs(4, 0);
            }
        );
        //isEntityInsideMaterial
        addMethod(classNode, "isEntityInsideMaterial", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;",
            "isEntityInsideFluid", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(DLOAD, 5);
                generator.visitVarInsn(ALOAD, 7);
                generator.visitVarInsn(ILOAD, 8);
                generator.visitMaxs(8, 0);
            }
        );
        //isAABBInsideMaterial
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
        //isAABBInsideLiquid
        addMethod(classNode, "isAABBInsideLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;",
            "isAABBInsideLiquid", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitMaxs(4, 0);
            }
        );
        //getFluid
        addMethod(classNode, "getFluid", "()Lnet/minecraftforge/fluids/Fluid;",
            "getLiquid", "(Lnet/minecraft/block/material/Material;)Lnet/minecraftforge/fluids/Fluid;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
                generator.visitMaxs(1, 0);
            }
        );
        //place
        addMethod(classNode, "place", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;Z)I",
            "place", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;ZLnet/minecraft/block/state/IBlockState;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
                generator.visitMethodInsn(INVOKESTATIC, "net/minecraft/block/BlockLiquid", obfuscated ? "func_176361_a" : "getFlowingBlock", "(Lnet/minecraft/block/material/Material;)Lnet/minecraft/block/BlockDynamicLiquid;", false);
                generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false);
                generator.visitMaxs(6, 0);
            }
        );
        //drain
        addMethod(classNode, "drain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Z)Lnet/minecraftforge/fluids/FluidStack;",
            "drain", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ZLnet/minecraftforge/fluids/FluidStack;)Lnet/minecraftforge/fluids/FluidStack;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
                generator.visitInsn(ACONST_NULL);
                generator.visitMaxs(5, 0);
            }
        );
        //canDrain
        addMethod(classNode, "canDrain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z",
            "canDrain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitMaxs(2, 0);
            }
        );
        //getFilledPercentage
        addMethod(classNode, "getFilledPercentage", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)F", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "getBlockLiquidHeight", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F", false);
            generator.visitMaxs(3, 0);
        });
        //isFluidloggableFluid
        addMethod(classNode, "isFluidloggableFluid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z",
            "isLiquidFluidloggable", "(Lgit/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitMaxs(4, 0);
            }
        );
        //isFluidloggableFluid
        addMethod(classNode, "isFluidloggableFluid", "()Z",
            "isLiquidFluidloggable", "(Lnet/minecraft/block/Block;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMaxs(1, 0);
            }
        );

        return true;
    }
}
