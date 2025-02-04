/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.transformers;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Many modded boat entities don't use AT's, and instead directly copy vanilla's boat code. This transformer attempts to
 * find all modded boat classes, and apply the proper transforms to make them FluidState-sensitive.
 * @author jbred
 *
 */
public final class TransformerModdedBoats implements IClassTransformer, IASMPlugin
{
    @Nullable
    @Override
    public byte[] transform(@Nonnull final String name, @Nonnull final String transformedName, @Nullable final byte[] basicClass) {
        if(basicClass == null) return null;

        // don't transform the vanilla boat class (handled by PluginEntityBoat)
        else if("net.minecraft.entity.item.EntityBoat".equals(transformedName)) return basicClass;
        @Nonnull final ClassNode classNode = new ClassNode();
        new ClassReader(basicClass).accept(classNode, 0);

        if(!"net/minecraft/entity/item/EntityBoat".equals(classNode.superName)) return basicClass;
        boolean wasClassTransformed = false;
        for(@Nonnull final MethodNode method : classNode.methods) {
            /*
             * updateFallState:
             * Old code:
             * IBlockState iblockstate = this.world.getBlockState(blockpos$pooledmutableblockpos);
             *
             * New code:
             * //account for FluidStates
             * IBlockState iblockstate = FluidloggedUtils.getFluidOrReal(this.world, blockpos$pooledmutableblockpos);
             */
            if(method.name.equals("func_184231_a") || method.name.equals("updateFallState") && method.desc.equals("(DZLnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/BlockPos;)V")) {
                for(@Nonnull final AbstractInsnNode insn : method.instructions.toArray()) {
                    if(insn instanceof MethodInsnNode && ((MethodInsnNode)insn).name.equals(FMLLaunchHandler.isDeobfuscatedEnvironment() ? "getBlockState" : "func_180495_p")) {
                        informConsole(transformedName, method);
                        method.instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
                        method.instructions.remove(insn);
                        wasClassTransformed = true;
                        break;
                    }
                }
            }
            // --------------------------------
            // call super for duplicate methods
            // --------------------------------
            else if(method.name.equals("func_184451_k") || method.name.equals("getWaterLevelAbove") && method.desc.equals("()F")) {
                overrideMethod(classNode, method, null, null, generator -> {
                    generator.visitVarInsn(ALOAD, 0);
                    generator.visitMethodInsn(INVOKESPECIAL, "net/minecraft/entity/item/EntityBoat", FMLLaunchHandler.isDeobfuscatedEnvironment() ? "getWaterLevelAbove" : "func_184451_k", "()F", false);
                });
                wasClassTransformed = true;
            }
            else if(method.name.equals("func_184446_u") || method.name.equals("checkInWater") && method.desc.equals("()Z")) {
                overrideMethod(classNode, method, null, null, generator -> {
                    // modded boat class copies vanilla's waterLevel field
                    if(classNode.fields.stream().anyMatch(field -> field.name.equals("waterLevel") && field.desc.equals("D"))) {
                        final int retVar = generator.newLocal(Type.BOOLEAN_TYPE);
                        generator.visitVarInsn(ALOAD, 0);
                        generator.visitMethodInsn(INVOKESPECIAL, "net/minecraft/entity/item/EntityBoat", FMLLaunchHandler.isDeobfuscatedEnvironment() ? "checkInWater" : "func_184446_u", "()Z", false);
                        generator.visitVarInsn(ISTORE, retVar);
                        generator.visitVarInsn(ALOAD, 0);
                        generator.visitVarInsn(ALOAD, 0);
                        generator.visitFieldInsn(GETFIELD, "net/minecraft/entity/item/EntityBoat", FMLLaunchHandler.isDeobfuscatedEnvironment() ? "waterLevel" : "field_184465_aD", "D");
                        generator.visitFieldInsn(PUTFIELD, classNode.name, "waterLevel", "D");
                        generator.visitVarInsn(ILOAD, retVar);
                    }
                    // modded boat class does not copy vanilla's waterLevel field
                    else {
                        generator.visitVarInsn(ALOAD, 0);
                        generator.visitMethodInsn(INVOKESPECIAL, "net/minecraft/entity/item/EntityBoat", FMLLaunchHandler.isDeobfuscatedEnvironment() ? "checkInWater" : "func_184446_u", "()Z", false);
                    }
                });
                wasClassTransformed = true;
            }
            else if(method.name.equals("func_184444_v") || method.name.equals("getUnderwaterStatus") && method.desc.endsWith("$Status;")) {
                overrideMethod(classNode, method, null, null, generator -> {
                    @Nonnull final Type localStatusEnum = Type.getReturnType(method.desc); // most modded boats also copy vanilla's boat "status" enum...
                    generator.visitMethodInsn(INVOKESTATIC, localStatusEnum.getInternalName(), "values", "()[" + localStatusEnum.getDescriptor(), false);
                    generator.visitVarInsn(ALOAD, 0);
                    generator.visitMethodInsn(INVOKESPECIAL, "net/minecraft/entity/item/EntityBoat", FMLLaunchHandler.isDeobfuscatedEnvironment() ? "getUnderwaterStatus" : "func_184444_v", "()Lnet/minecraft/entity/item/EntityBoat$Status;", false);
                    generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "getOrNull", "([Ljava/lang/Object;Ljava/lang/Enum;)Ljava/lang/Object;", false);
                    generator.visitTypeInsn(CHECKCAST, localStatusEnum.getInternalName());
                });
                wasClassTransformed = true;
            }
        }

        if(wasClassTransformed) {
            @Nonnull final ClassWriter writer = new ClassWriter(0);
            classNode.accept(writer);
            return writer.toByteArray();
        }

        else return basicClass;
    }

    @Override
    public void informConsole(@Nonnull final String className, @Nullable final MethodNode method) {
        IASMPlugin.setActivePlugin("Fluidlogged API Plugin");
        IASMPlugin.super.informConsole(className, method);
        IASMPlugin.resetActivePlugin();
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static Object getOrNull(@Nonnull final Object[] a, @Nullable final Enum<?> e) { return e == null ? null : a[e.ordinal()]; }
    }
}
