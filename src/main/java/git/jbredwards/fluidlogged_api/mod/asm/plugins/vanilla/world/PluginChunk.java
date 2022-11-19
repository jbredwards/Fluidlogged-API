package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.impl.IFluidStatePrimer;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
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
        if(method.name.equals("<init>")) return 1;
        else if(checkMethod(method, obfuscated ? "func_150808_b" : "getBlockLightOpacity", "(III)I")) return 2;
        else if(method.name.equals(obfuscated ? "func_177436_a" : "setBlockState")) return 3;
        else if(method.name.equals(obfuscated ? "func_76594_o" : "enqueueRelightChecks")) return 4;
        else return checkMethod(method, obfuscated ? "func_150811_f" : "checkLight", "(II)Z") ? 5 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
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
        if(index == 1 && insn.getOpcode() == INVOKEVIRTUAL) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(genMethodNode("generateFluidStates", "(Lnet/minecraft/world/chunk/Chunk;Lgit/jbredwards/fluidlogged_api/api/asm/impl/IFluidStatePrimer;)V"));
            instructions.insert(insn.getNext(), list);
            return true;
        }
        /*
         * getBlockLightOpacity (changes are around line 510):
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
         * setBlockState (changes are around lines 592 & 639):
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
         * enqueueRelightChecks: (changes are around lines 1384)
         * Old code:
         * if (this.world.getBlockState(blockpos2).getLightValue(this.world, blockpos2) > 0)
         * {
         *     ...
         * }
         *
         * New code:
         * //account for FluidState light value
         * if (Hooks.getFluidLightValue(this.world, blockpos2) > 0)
         * {
         *     ...
         * }
         */
        else if(index == 4 && checkMethod(insn, "getLightValue")) {
            removeFrom(instructions, getPrevious(insn, 3), -3);
            instructions.insert(insn, genMethodNode("getFluidLightValue", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I"));
            instructions.remove(insn);
            return true;
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
        else if(index == 5 && checkMethod(insn, "getLightValue")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insert(insn, genMethodNode("getFluidLightValue", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.remove(insn);
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

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void generateFluidStates(@Nonnull Chunk chunk, @Nonnull IFluidStatePrimer primer) {
            final IFluidStateCapability cap = IFluidStateCapability.get(chunk);
            if(cap != null) {
                IFluidStateContainer container = cap.getContainer(chunk.x, 0, chunk.z);
                for(int y = 0; y < 256; y++) {
                    //initialize container for new chunkY
                    if(y != 0 && (y & 15) == 0) container = cap.getContainer(chunk.x, y >> 4, chunk.z);
                    for(int x = 0; x < 16; x++) {
                        for(int z = 0; z < 16; z++) {
                            final FluidState fluidState = primer.getFluidState(x, y, z);
                            if(!fluidState.isEmpty()) {
                                final BlockPos pos = new BlockPos(chunk.x << 4 | x, y, chunk.z << 4 | z);
                                container.setFluidState(pos, fluidState);
                            }
                        }
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

        public static int getFluidLightValue(@Nonnull World world, @Nonnull BlockPos pos) {
            final Chunk chunk = world.getChunk(pos);
            return getFluidLightValue(chunk.getBlockState(pos), world, pos, chunk);
        }

        public static int getFluidLightValue(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return Math.max(state.getLightValue(world, pos), FluidState.getFromProvider(chunk, pos).getState().getLightValue(world, pos));
        }
    }
}
