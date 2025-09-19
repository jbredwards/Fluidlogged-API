/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.asm.transformers;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Modifier;
import java.util.ArrayList;

/**
 * - change all calls from Block.canConnectRedstone(state, world, pos, side) to Hooks.canConnectRedstone(block, state, world, pos, side), this allows FluidStates to connect to redstone components
 * - change all calls from Block.getExplosionResistance(world, pos, entity, explosion) to Hooks.getExplosionResistance(block, world, pos, entity, explosion), this allows FluidStates to resist explosions
 * @author jbred
 *
 */
public final class TransformerMethodRedirects implements IClassTransformer
{
    @Nullable
    @Override
    public byte[] transform(@Nonnull final String name, @Nonnull final String transformedName, @Nullable final byte[] basicClass) {
        if(basicClass == null) return null;

        // don't transform the hook method, that needs to keep the original call
        else if(transformedName.startsWith("git.jbredwards.fluidlogged_api.mod.asm.transformers")) return basicClass;
        @Nonnull final ClassNode classNode = new ClassNode();
        new ClassReader(basicClass).accept(classNode, 0);

        boolean wasClassTransformed = false;
        for(@Nonnull final MethodNode method : new ArrayList<>(classNode.methods)) {
            for(@Nonnull final AbstractInsnNode insn : method.instructions.toArray()) {
                if(insn.getOpcode() == Opcodes.INVOKEVIRTUAL) {
                    if("canConnectRedstone".equals(((MethodInsnNode)insn).name) && "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z".equals(((MethodInsnNode)insn).desc)) {
                        method.instructions.insert(insn, new MethodInsnNode(Opcodes.INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/asm/transformers/TransformerMethodRedirects$Hooks", "canConnectRedstone", "(Lgit/jbredwards/fluidlogged_api/mod/asm/transformers/TransformerMethodRedirects$ConnectAccessor;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z", false));
                        method.instructions.remove(insn);
                        wasClassTransformed = true;
                    }
                    else if("getExplosionResistance".equals(((MethodInsnNode)insn).name) && "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/Entity;Lnet/minecraft/world/Explosion;)F".equals(((MethodInsnNode)insn).desc)) {
                        method.instructions.insert(insn, new MethodInsnNode(Opcodes.INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/asm/transformers/TransformerMethodRedirects$Hooks", "getExplosionResistance", "(Lgit/jbredwards/fluidlogged_api/mod/asm/transformers/TransformerMethodRedirects$ResistanceAccessor;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/Entity;Lnet/minecraft/world/Explosion;)F", false));
                        method.instructions.remove(insn);
                        wasClassTransformed = true;
                    }
                    // injectors
                    else if(!Modifier.isStatic(method.access)) {
                        if(FMLLaunchHandler.isDeobfuscatedEnvironment() ? "containsAnyLiquid".equals(((MethodInsnNode)insn).name) && "(Lnet/minecraft/util/math/AxisAlignedBB;)Z".equals(((MethodInsnNode)insn).desc) : "func_72953_d".equals(((MethodInsnNode)insn).name)) {
                            method.instructions.insert(insn, new MethodInsnNode(Opcodes.INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/asm/transformers/TransformerMethodRedirects$Hooks", "containsAnyLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/AxisAlignedBB;Ljava/lang/Object;)Z", false));
                            method.instructions.insert(insn, new VarInsnNode(Opcodes.ALOAD, 0));
                            method.instructions.remove(insn);
                            wasClassTransformed = true;
                        }
                        else if(FMLLaunchHandler.isDeobfuscatedEnvironment() ? "isMaterialInBB".equals(((MethodInsnNode)insn).name) && "(Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Z".equals(((MethodInsnNode)insn).desc) : "func_72875_a".equals(((MethodInsnNode)insn).name)) {
                            method.instructions.insert(insn, new MethodInsnNode(Opcodes.INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/asm/transformers/TransformerMethodRedirects$Hooks", "isMaterialInBB", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;Ljava/lang/Object;)Z", false));
                            method.instructions.insert(insn, new VarInsnNode(Opcodes.ALOAD, 0));
                            method.instructions.remove(insn);
                            wasClassTransformed = true;
                        }
                    }
                }
            }
            // accessors
            if(Modifier.isPublic(method.access) && !Modifier.isAbstract(method.access) && !Modifier.isStatic(method.access)) {
                if("canConnectRedstone".equals(method.name) && "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z".equals(method.desc)) {
                    @Nonnull final MethodNode accessor = new MethodNode(Opcodes.ACC_PUBLIC, "fluidlogged_api$canConnectRedstone", method.desc, null, null);
                    accessor.visitVarInsn(Opcodes.ALOAD, 0);
                    accessor.visitVarInsn(Opcodes.ALOAD, 1);
                    accessor.visitVarInsn(Opcodes.ALOAD, 2);
                    accessor.visitVarInsn(Opcodes.ALOAD, 3);
                    accessor.visitVarInsn(Opcodes.ALOAD, 4);
                    accessor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, classNode.name, method.name, method.desc, false);
                    accessor.visitInsn(Opcodes.IRETURN);

                    wasClassTransformed = true;
                    classNode.methods.add(accessor);
                    classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/transformers/TransformerMethodRedirects$ConnectAccessor");
                }
                else if("getExplosionResistance".equals(method.name) && "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/Entity;Lnet/minecraft/world/Explosion;)F".equals(method.desc)) {
                    @Nonnull final MethodNode accessor = new MethodNode(Opcodes.ACC_PUBLIC, "fluidlogged_api$getExplosionResistance", method.desc, null, null);
                    accessor.visitVarInsn(Opcodes.ALOAD, 0);
                    accessor.visitVarInsn(Opcodes.ALOAD, 1);
                    accessor.visitVarInsn(Opcodes.ALOAD, 2);
                    accessor.visitVarInsn(Opcodes.ALOAD, 3);
                    accessor.visitVarInsn(Opcodes.ALOAD, 4);
                    accessor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, classNode.name, method.name, method.desc, false);
                    accessor.visitInsn(Opcodes.FRETURN);

                    wasClassTransformed = true;
                    classNode.methods.add(accessor);
                    classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/transformers/TransformerMethodRedirects$ResistanceAccessor");
                }
            }
        }

        if(wasClassTransformed) {
            @Nonnull final ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_MAXS);
            classNode.accept(writer);
            return writer.toByteArray();
        }

        else return basicClass;
    }

    @SuppressWarnings("unused")
    public interface ConnectAccessor
    {
        boolean fluidlogged_api$canConnectRedstone(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nullable final EnumFacing side);
    }

    @SuppressWarnings("unused")
    public interface ResistanceAccessor
    {
        float fluidlogged_api$getExplosionResistance(@Nullable final World world, @Nullable final BlockPos pos, @Nullable final Entity exploder, @Nullable final Explosion explosion);
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canConnectRedstone(@Nonnull final ConnectAccessor block, @Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nullable final EnumFacing side) {
            final boolean bCanConnect = block.fluidlogged_api$canConnectRedstone(state, world, pos, side);
            if(bCanConnect || !(block instanceof Block)) return bCanConnect;

            else if(side == null || !FluidloggedAPIConfig.fixBadFluidMixing || FluidloggedUtils.canFluidFlow(world, pos, state, side.getOpposite())) {
                @Nonnull final IBlockState fluidState = FluidState.get(world, pos).getState();
                return fluidState.getBlock().canConnectRedstone(fluidState, world, pos, side);
            }

            else return false;
        }

        public static float getExplosionResistance(@Nonnull final ResistanceAccessor block, @Nullable final World world, @Nullable final BlockPos pos, @Nullable final Entity exploder, @Nullable final Explosion explosion) {
            final float bResistance = block.fluidlogged_api$getExplosionResistance(world, pos, exploder, explosion);
            if(world == null || pos == null || explosion == null || bResistance >= Integer.MAX_VALUE || !(block instanceof Block) || FluidloggedUtils.isFluid((Block)block)) return bResistance;

            final float fResistance = FluidState.get(world, pos).getBlock().getExplosionResistance(world, pos, exploder, explosion);
            if(fResistance >= Integer.MAX_VALUE) return fResistance;

            else return bResistance + fResistance < 0 ? Float.MAX_VALUE : bResistance + fResistance;
        }

        // injectors

        @Nullable
        public static Object currentEntity;
        public static boolean containsAnyLiquid(@Nonnull final World world, @Nonnull final AxisAlignedBB bb, @Nonnull final Object thi5) {
            currentEntity = thi5;
            final boolean ret = world.containsAnyLiquid(bb);
            currentEntity = null;
            return ret;
        }

        public static boolean isMaterialInBB(@Nonnull final World world, @Nonnull final AxisAlignedBB bb, @Nonnull final Material materialIn, @Nonnull final Object thi5) {
            currentEntity = thi5;
            final boolean ret = world.isMaterialInBB(bb, materialIn);
            currentEntity = null;
            return ret;
        }
    }
}
