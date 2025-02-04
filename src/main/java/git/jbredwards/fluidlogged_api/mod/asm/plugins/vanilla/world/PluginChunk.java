/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.world.IFluidStatePrimer;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IHardcodedCapability;
import git.jbredwards.fluidlogged_api.mod.common.capability.FluidStateCapabilityVanilla;
import git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks.FluidStateCapabilityIColumn;
import io.github.opencubicchunks.cubicchunks.api.world.IColumn;
import io.github.opencubicchunks.cubicchunks.api.world.ICubicWorld;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * account for FluidState light opacity & light values
 * @author jbred
 *
 */
public final class PluginChunk implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals("<init>") && method.desc.equals("(Lnet/minecraft/world/World;II)V")) return 7;
        else if(method.name.equals("<init>") && method.desc.equals("(Lnet/minecraft/world/World;Lnet/minecraft/world/chunk/ChunkPrimer;II)V")) return 1;
        else if(checkMethod(method, obfuscated ? "func_150808_b" : "getBlockLightOpacity", "(III)I")) return 2;
        else if(method.name.equals(obfuscated ? "func_177436_a" : "setBlockState")) return 3;
        else if(method.name.equals(obfuscated ? "func_177440_h" : "getPrecipitationHeight")) return 4;
        else if(method.name.equals(obfuscated ? "func_76594_o" : "enqueueRelightChecks")) return 5;
        else if(checkMethod(method, obfuscated ? "func_186030_a" : "populate", "(Lnet/minecraft/world/chunk/IChunkProvider;Lnet/minecraft/world/gen/IChunkGenerator;)V")) return 8;
        else return checkMethod(method, obfuscated ? "func_150811_f" : "checkLight", "(II)Z") ? 6 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * Constructor: (changes are around line 95)
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * // create hardcoded capability instance
         * {
         *     Hooks.createCapabilityInstance(this);
         *     ...
         * }
         */
        if(index == 7 && insn.getOpcode() == INVOKESPECIAL) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(genMethodNode("createCapabilityInstance", "(Lnet/minecraft/world/chunk/Chunk;)V"));
            instructions.insert(insn.getNext(), list);
            return true;
        }
        /*
         * Constructor: (changes are around line 122)
         * Old code:
         * boolean flag = worldIn.provider.hasSkyLight();
         *
         * New code:
         * //generate FluidStates stored in primer
         * boolean flag = worldIn.provider.hasSkyLight();
         * Hooks.generateFluidStates(this, primer);
         */
        else if(index == 1 && insn.getOpcode() == INVOKEVIRTUAL) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(genMethodNode("generateFluidStates", "(Lnet/minecraft/world/chunk/Chunk;Lgit/jbredwards/fluidlogged_api/api/world/IFluidStatePrimer;)V"));
            instructions.insert(insn.getNext(), list);
            return true;
        }
        /*
         * getBlockLightOpacity: (changes are around line 510)
         * Old code:
         * return !loaded ? state.getLightOpacity() : state.getLightOpacity(world, new BlockPos(this.x << 4 | x & 15, y, this.z << 4 | z & 15));
         *
         * New code:
         * //account for FluidState light opacity
         * return !loaded ? Hooks.getFluidLightOpacity(state, this, x, y, z) : Hooks.getFluidLightOpacity(state, this.world, new BlockPos(this.x << 4 | x & 15, y, this.z << 4 | z & 15), this);
         */
        else if(index == 2) {
            if(checkMethod(insn, obfuscated ? "func_185891_c" : "getLightOpacity", "()I")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new VarInsnNode(ILOAD, 1));
                instructions.insertBefore(insn, new VarInsnNode(ILOAD, 2));
                instructions.insertBefore(insn, new VarInsnNode(ILOAD, 3));
                instructions.insertBefore(insn, genMethodNode("getFluidLightOpacity", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/chunk/Chunk;III)I"));
                instructions.remove(insn);
            }
            else if(checkMethod(insn, "getLightOpacity")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, genMethodNode("getFluidLightOpacity", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
                instructions.remove(insn);
                return true;
            }
        }
        /*
         * setBlockState: (changes are around lines 592 & 639)
         * Old code:
         * int k1 = iblockstate.getLightOpacity(this.world, pos);
         *
         * New code:
         * //account for FluidStates when updating the light level
         * int k1 = Hooks.getFluidLightOpacity(iblockstate, this.world, pos, this);
         */
        else if(index == 3 && checkMethod(insn, "getLightOpacity")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, genMethodNode("getFluidLightOpacity", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.remove(insn);
        }
        /*
         * setBlockState: (changes are around lines 1119)
         * Old code:
         * if (!material.blocksMovement() && !material.isLiquid())
         * {
         *     ...
         * }
         *
         * New code:
         * //account for FluidStates and *any* fluid blocks
         * if (!material.blocksMovement() && !Hooks.hasFluidAt(this, blockpos, iblockstate))
         * {
         *     ...
         * }
         */
        else if(index == 4 && checkMethod(insn, obfuscated ? "func_76224_d" : "isLiquid")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 5));
            list.add(new VarInsnNode(ALOAD, 8));
            list.add(genMethodNode("hasFluidAt", "(Lnet/minecraft/world/chunk/Chunk;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z"));
            instructions.insert(insn, list);
            removeFrom(instructions, insn, -1);
            return true;
        }
        //enqueueRelightChecks
        else if(index == 5) {
            /*
             * enqueueRelightChecks: (changes are around lines 1378)
             * Old code:
             * if (this.storageArrays[j] == NULL_BLOCK_STORAGE && flag || this.storageArrays[j] != NULL_BLOCK_STORAGE && this.storageArrays[j].get(k, i1, l).getBlock().isAir(this.storageArrays[j].get(k, i1, l), this.world, blockpos1))
             * {
             *     ...
             * }
             *
             * New code:
             * //allow fluid blocks to enqueue relight checks
             * if (this.storageArrays[j] == NULL_BLOCK_STORAGE && flag || this.storageArrays[j] != NULL_BLOCK_STORAGE && Hooks.isFluidOrAir(this.storageArrays[j].get(k, i1, l), this.world, blockpos1)))
             * {
             *     ...
             * }
             */
            if(checkMethod(insn, "isAir")) {
                instructions.insert(insn, genMethodNode("isFluidOrAir", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
                removeFrom(instructions, getPrevious(insn, 4), -8);
                instructions.remove(insn);
            }
            /*
             * enqueueRelightChecks: (changes are around lines 1384)
             * Old code:
             * if (this.world.getBlockState(blockpos2).getLightValue(this.world, blockpos2) > 0)
             * {
             *     ...
             * }
             *
             * New code:
             * //account for FluidState light value
             * if (Hooks.getFluidLightValue(this.world, blockpos2, this) > 0)
             * {
             *     ...
             * }
             */
            else if(checkMethod(insn, "getLightValue")) {
                removeFrom(instructions, getPrevious(insn, 3), -3);
                instructions.insert(insn, genMethodNode("getFluidLightValue", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
                instructions.insert(insn, new VarInsnNode(ALOAD, 0));
                instructions.remove(insn);
                return true;
            }
        }
        /*
         * checkLight: (changes are around line 1515)
         * Old code:
         * if (this.getBlockState(blockpos$mutableblockpos).getLightValue(this.world, blockpos$mutableblockpos) > 0)
         * {
         *     ...
         * }
         *
         * New code:
         * //account for FluidState light value
         * if (Hooks.getFluidLightValue(this.getBlockState(blockpos$mutableblockpos), this.world, blockpos$mutableblockpos, this)) > 0)
         * {
         *     ...
         * }
         */
        else if(index == 6 && checkMethod(insn, "getLightValue")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insert(insn, genMethodNode("getFluidLightValue", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.remove(insn);
            return true;
        }
        /*
         * populate (changes are around lines 1047 and 1076):
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * //
         * {
         *     FluidState.removeOnBlockChange.set(Boolean.TRUE);
         *     ...
         *     FluidState.removeOnBlockChange.set(Boolean.FALSE);
         * }
         */
        else if(index == 8) {
            @Nonnull final InsnList before = new InsnList();
            before.add(new FieldInsnNode(GETSTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "removeOnBlockChange", "Ljava/lang/ThreadLocal;"));
            before.add(new FieldInsnNode(GETSTATIC, "java/lang/Boolean", "TRUE", "Ljava/lang/Boolean;"));
            before.add(new MethodInsnNode(INVOKEVIRTUAL, "java/lang/ThreadLocal", "set", "(Ljava/lang/Object;)V", false));
            instructions.insert(instructions.getFirst().getNext(), before);

            @Nonnull final InsnList after = new InsnList();
            after.add(new FieldInsnNode(GETSTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "removeOnBlockChange", "Ljava/lang/ThreadLocal;"));
            after.add(new FieldInsnNode(GETSTATIC, "java/lang/Boolean", "FALSE", "Ljava/lang/Boolean;"));
            after.add(new MethodInsnNode(INVOKEVIRTUAL, "java/lang/ThreadLocal", "set", "(Ljava/lang/Object;)V", false));
            instructions.insertBefore(instructions.getLast().getPrevious(), after);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * getBlockLightOpacity
         * New code:
         * //call equivalent method instead of using worse duplicate code
         * public int getBlockLightOpacity(BlockPos pos)
         * {
         *     return this.getBlockLightOpacity(pos.getX(), pos.getY(), pos.getZ());
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_177437_b" : "getBlockLightOpacity", "(Lnet/minecraft/util/math/BlockPos;)I"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/util/math/BlockPos", obfuscated ? "func_177958_n" : "getX", "()I", false);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/util/math/BlockPos", obfuscated ? "func_177956_o" : "getY", "()I", false);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/util/math/BlockPos", obfuscated ? "func_177952_p" : "getZ", "()I", false);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/chunk/Chunk", obfuscated ? "func_150808_b" : "getBlockLightOpacity", "(III)I", false);
        });
        /*
         * =========
         * Accessors
         * =========
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/IHardcodedCapability");
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "fluidStateCapability", "Lgit/jbredwards/fluidlogged_api/api/capability/IFluidStateCapability;", null, null));
        /*
         * Accessor:
         * New code:
         * // getter for canFluidFlow
         * @ASMGenerated
         * public IFluidStateCapability getFluidStateCapability()
         * {
         *     return this.fluidStateCapability;
         * }
         */
        addMethod(classNode, "getFluidStateCapability", "()Lgit/jbredwards/fluidlogged_api/api/capability/IFluidStateCapability;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/world/chunk/Chunk", "fluidStateCapability", "Lgit/jbredwards/fluidlogged_api/api/capability/IFluidStateCapability;");
        });
        /*
         * Accessor:
         * New code:
         * // setter for canFluidFlow
         * @ASMGenerated
         * public void setFluidStateCapability(IFluidStateCapability fluidStateCapability)
         * {
         *     this.fluidStateCapability = fluidStateCapability;
         * }
         */
        addMethod(classNode, "setFluidStateCapability", "(Lgit/jbredwards/fluidlogged_api/api/capability/IFluidStateCapability;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/world/chunk/Chunk", "fluidStateCapability", "Lgit/jbredwards/fluidlogged_api/api/capability/IFluidStateCapability;");
        });

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void createCapabilityInstance(@Nonnull final Chunk chunk) {
            // create hardcoded capability instance
            final boolean cubic = FluidloggedAPI.isCubicChunks && CCHooks.isColumn(chunk) && CCHooks.isCubicWorld(chunk.getWorld());
            ((IHardcodedCapability)chunk).setFluidStateCapability(cubic ? new FluidStateCapabilityIColumn(chunk) : new FluidStateCapabilityVanilla(chunk.x, chunk.z));
        }

        public static void generateFluidStates(@Nonnull Chunk chunk, @Nonnull IFluidStatePrimer primer) {
            final IFluidStateCapability cap = ((IHardcodedCapability)chunk).getFluidStateCapability();
            // generate fluidStates from primer
            IFluidStateContainer container = null;
            for(int y = 0; y < 256; y++) {
                if((y & 15) == 0) container = cap.getContainer(y);
                for(int x = 0; x < 16; x++) {
                    for(int z = 0; z < 16; z++) {
                        final FluidState fluidState = primer.getFluidState(x, y, z);
                        if(fluidState != FluidState.EMPTY) container.setFluidState(chunk.x << 4 | x, y, chunk.z << 4 | z, fluidState);
                    }
                }
            }
        }

        public static int getFluidLightOpacity(@Nonnull IBlockState state, @Nonnull Chunk chunk, int x, int y, int z) {
            return Math.max(state.getLightOpacity(), FluidState.getFromProvider(chunk,
                    new BlockPos(chunk.x << 4 | x & 15, y, chunk.z << 4 | z & 15)).getState().getLightOpacity());
        }

        public static int getFluidLightOpacity(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return Math.max(state.getLightOpacity(world, pos), FluidState.getFromProvider(chunk, pos).getState().getLightOpacity(world, pos));
        }

        public static int getFluidLightValue(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunkIn) {
            final Chunk chunk = chunkIn.isAtLocation(pos.getX() >> 4, pos.getZ() >> 4) ? chunkIn : world.getChunk(pos);
            return getFluidLightValue(chunk.getBlockState(pos), world, pos, chunk);
        }

        public static int getFluidLightValue(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return Math.max(state.getLightValue(world, pos), FluidState.getFromProvider(chunk, pos).getState().getLightValue(world, pos));
        }

        public static boolean hasFluidAt(@Nonnull Chunk chunk, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
            return FluidloggedUtils.isFluid(state) || !FluidState.getFromProvider(chunk, pos).isEmpty();
        }

        public static boolean isFluidOrAir(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos) {
            return state.getBlock().isAir(state, world, pos) || FluidloggedUtils.isFluid(state);
        }
    }

    //hold Cubic Chunks methods in separate class to avoid crash
    public static final class CCHooks
    {
        public static boolean isColumn(@Nonnull final Chunk chunk) {
            return chunk instanceof IColumn && isCubicWorld(chunk.getWorld());
        }

        public static boolean isCubicWorld(@Nonnull final World world) {
            return world instanceof ICubicWorld && ((ICubicWorld)world).isCubicWorld();
        }
    }
}
