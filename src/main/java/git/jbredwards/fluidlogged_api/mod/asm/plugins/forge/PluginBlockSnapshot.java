/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.BlockSnapshot;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * save FluidStates in block snapshots
 * @author jbred
 *
 */
public final class PluginBlockSnapshot implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(checkMethod(method, "<init>", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/nbt/NBTTagCompound;)V")) return 1;
        // else if(checkMethod(method, "<init>", "(ILnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/ResourceLocation;IILnet/minecraft/nbt/NBTTagCompound;)V")) return 2;
        else if(method.name.equals("readFromNBT")) return 3;
        else if(method.name.equals("writeToNBT")) return 5;
        else if(method.name.equals("equals")) return 6;
        else return method.name.equals("restoreToLocation") ? 4 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Constructor: (changes are around line )
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * // Save FluidState
         * {
         *     ...
         *     Hooks.setReplacedFluid(this, world, pos);
         * }
         */
        if(index == 1 && insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, genMethodNode("setReplacedFluid", withAccessorClass("(L%s;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V")));
            return true;
        }
        /*
         * readFromNBT: (changes are around line 124)
         * Old code:
         * return ...
         *
         * New code:
         * // Read FluidState
         * return Hooks.getBlockSnapshot(..., tag);
         */
        else if(index == 3 && insn.getOpcode() == ARETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, genMethodNode("getBlockSnapshot", "(Lnet/minecraftforge/common/util/BlockSnapshot;Lnet/minecraft/nbt/NBTTagCompound;)Lnet/minecraftforge/common/util/BlockSnapshot;"));
            return true;
        }
        /*
         * restoreToLocation: (changes are around lines 193 & 201)
         * Old code:
         * world.setBlockState(pos, replaced, flags);
         *
         * New code:
         * // Restore FluidState
         * world.setBlockState(pos, replaced, flags);
         * Hooks.restoreToLocation(this, world, pos);
         */
        else if(index == 4 && checkMethod(insn.getPrevious(), obfuscated ? "func_180501_a" : "setBlockState")) {
            @Nonnull final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(genMethodNode("restoreToLocation", "(Lnet/minecraftforge/common/util/BlockSnapshot;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V"));
            instructions.insert(insn, list);
        }
        /*
         * writeToNBT: (changes are around line 238)
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * // Write FluidState
         * {
         *     ...
         *     Hooks.writeToNBT(this, compound);
         * }
         */
        else if(index == 5 && insn.getOpcode() == RETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, genMethodNode("writeToNBT", withAccessorClass("(L%s;Lnet/minecraft/nbt/NBTTagCompound;)V")));
            return true;
        }
        /*
         * equals: (changes are around line 277)
         * Old code:
         * return true;
         *
         * New code:
         * // Account for "replacedFluid"
         * return Hooks.equals(true, this, (Accessor)o);
         */
        else if(index == 6 && insn.getOpcode() == IRETURN) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new TypeInsnNode(CHECKCAST, getAccessorClass()));
            instructions.insertBefore(insn, genMethodNode("equals", withAccessorClass("(ZL%s;L%s;)Z")));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "replacedFluid", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null));
        // Accessor for "replacedFluid"
        classNode.interfaces.add(getAccessorClass());
        addMethod(classNode, "getReplacedFluid", "()Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/common/util/BlockSnapshot", "replacedFluid", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });
        addMethod(classNode, "setReplacedFluid", "(Lgit/jbredwards/fluidlogged_api/api/util/FluidState;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraftforge/common/util/BlockSnapshot", "replacedFluid", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });
        /*
         * New code:
         * // Save FluidState
         * @ASMOverwrite
         * public static BlockSnapshot getBlockSnapshot(World world, BlockPos pos)
         * {
         *     return Hooks.getBlockSnapshot(world, pos);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, "getBlockSnapshot", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraftforge/common/util/BlockSnapshot;"),
            "getBlockSnapshot", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraftforge/common/util/BlockSnapshot;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
            }
        );
        /*
         * New code:
         * // Save FluidState
         * @ASMOverwrite
         * public static BlockSnapshot getBlockSnapshot(World world, BlockPos pos, int flags)
         * {
         *     return Hooks.getBlockSnapshot(world, pos, flags);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, "getBlockSnapshot", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)Lnet/minecraftforge/common/util/BlockSnapshot;"),
            "getBlockSnapshot", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)Lnet/minecraftforge/common/util/BlockSnapshot;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ILOAD, 2);
            }
        );

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static final ThreadLocal<Boolean> gatheringFluids = ThreadLocal.withInitial(() -> Boolean.FALSE);

        @Nonnull
        public static BlockSnapshot getBlockSnapshot(@Nonnull final World world, @Nonnull final BlockPos pos) {
            gatheringFluids.set(Boolean.TRUE);
            @Nonnull final Chunk chunk = world.getChunk(pos);
            @Nonnull final BlockSnapshot snapshot = new BlockSnapshot(world, pos, chunk.getBlockState(pos));
            ((Accessor)snapshot).setReplacedFluid(FluidState.getFromProvider(chunk, pos));
            gatheringFluids.set(Boolean.FALSE);
            return snapshot;
        }

        @Nonnull
        public static BlockSnapshot getBlockSnapshot(@Nonnull final World world, @Nonnull final BlockPos pos, final int flags) {
            gatheringFluids.set(Boolean.TRUE);
            @Nonnull final Chunk chunk = world.getChunk(pos);
            @Nonnull final BlockSnapshot snapshot = new BlockSnapshot(world, pos, chunk.getBlockState(pos), flags);
            ((Accessor)snapshot).setReplacedFluid(FluidState.getFromProvider(chunk, pos));
            gatheringFluids.set(Boolean.FALSE);
            return snapshot;
        }

        @Nonnull
        public static BlockSnapshot getBlockSnapshot(@Nonnull final BlockSnapshot snapshot, @Nonnull final NBTTagCompound nbt) {
            @Nullable final Block fluidBlock = Block.getBlockFromName(nbt.getString("fluidBlock"));
            if(fluidBlock != null) ((Accessor)snapshot).setReplacedFluid(FluidState.of(fluidBlock.getStateFromMeta(nbt.getInteger("fluidMeta"))));
            return snapshot;
        }

        public static boolean equals(final boolean ret, @Nonnull final Accessor snapshot, @Nonnull final Accessor other) {
            if(!ret) return false;
            if(snapshot.getReplacedFluid() == other.getReplacedFluid()) return true;
            else if(snapshot.getReplacedFluid() == FluidState.EMPTY && other.getReplacedFluid() == null) return true;
            else return snapshot.getReplacedFluid() == null && other.getReplacedFluid() == FluidState.EMPTY;
        }

        public static void restoreToLocation(@Nonnull final BlockSnapshot snapshot, @Nonnull final World world, @Nonnull final BlockPos pos) {
            @Nullable final FluidState replacedFluid = ((Accessor)snapshot).getReplacedFluid();
            if(replacedFluid != null) FluidloggedUtils.setFluidState(world, pos, snapshot.getReplacedBlock(), replacedFluid, false, snapshot.getFlag());
        }

        public static void setReplacedFluid(@Nonnull final Accessor snapshot, @Nonnull final World world, @Nonnull final BlockPos pos) {
            if(!gatheringFluids.get()) snapshot.setReplacedFluid(FluidState.get(world, pos));
        }

        public static void writeToNBT(@Nonnull final Accessor snapshot, @Nonnull final NBTTagCompound nbt) {
            @Nullable final FluidState replacedFluid = snapshot.getReplacedFluid();
            if(replacedFluid != null) {
                nbt.setString("fluidBlock", replacedFluid.getBlock().getRegistryName().toString());
                nbt.setInteger("fluidMeta", replacedFluid.getMetadata());
            }
        }
    }

    public interface Accessor
    {
        @Nullable
        FluidState getReplacedFluid();
        void setReplacedFluid(@Nonnull final FluidState replacedFluid);
    }
}
