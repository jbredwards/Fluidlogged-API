package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * use FluidState light opacity
 * @author jbred
 *
 */
public final class PluginChunk implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, obfuscated ? "func_150808_b" : "getBlockLightOpacity", "(III)I")) return 1;
        return method.name.equals(obfuscated ? "func_177436_a" : "setBlockState") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getBlockLightOpacity (changes are around line 510):
         * Old code:
         * return !loaded ? state.getLightOpacity() : state.getLightOpacity(world, new BlockPos(this.x << 4 | x & 15, y, this.z << 4 | z & 15));
         *
         * New code:
         * //account for FluidState light opacity
         * return !loaded ? Hooks.getFluidLightOpacity(state, this, x, y, z) : Hooks.getFluidLightOpacity(state, this.world, new BlockPos(this.x << 4 | x & 15, y, this.z << 4 | z & 15), this);
         */
        if(index == 1) {
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
        else if(index == 2 && checkMethod(insn, "getLightOpacity")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, genMethodNode("getFluidLightOpacity", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.remove(insn);
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
        public static int getFluidLightOpacity(@Nonnull IBlockState state, @Nonnull Chunk chunk, int x, int y, int z) {
            return Math.max(state.getLightOpacity(), FluidState.getFromProvider(chunk,
                    new BlockPos(chunk.x << 4 | x & 15, y, chunk.z << 4 | z & 15)).getState().getLightOpacity());
        }

        public static int getFluidLightOpacity(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return Math.max(state.getLightOpacity(world, pos), FluidState.getFromProvider(chunk, pos).getState().getLightOpacity(world, pos));
        }
    }
}
