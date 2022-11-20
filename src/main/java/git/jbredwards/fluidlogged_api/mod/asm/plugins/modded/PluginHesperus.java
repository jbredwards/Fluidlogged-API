package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.PluginChunk;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * phosphor takes FluidStates into account when computing light
 * @author jbred
 *
 */
public final class PluginHesperus implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        switch(method.name) {
            case "initChunkLighting": return 1;
            case "processLightUpdatesForTypeInner": return 2;
            case "spreadLightFromCursor": return 3;
            case "getCursorLuminosity": return 4;
            default: return method.name.equals("getPosOpacity") ? 5 : 0;
        }
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * initChunkLighting:
         * Old code:
         * int light = LightingEngineHelpers.getLightValueForState(state, world, pos);
         *
         * New code:
         * //account for FluidState
         * int light = Hooks.compareWithFluid(LightingEngineHelpers.getLightValueForState(state, world, pos), world, pos, chunk);
         */
        if(index == 1 && checkMethod(insn, "getLightValueForState")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 4));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(genMethodNode("compareWithFluid", "(ILnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.insert(insn, list);
            return true;
        }
        /*
         * processLightUpdatesForTypeInner:
         * Old code:
         * if (curLight - this.getPosOpacity(nPos, LightingEngineHelpers.posToState(nPos, info.section)) >= nLight)
         * {
         *     ...
         * }
         *
         * New code:
         * //account for FluidState
         * if (curLight - Hooks.getNeighborOpacity(nPos, LightingEngineHelpers.posToState(nPos, info.section), this.world, nChunk) >= nLight)
         * {
         *     ...
         * }
         */
        else if(index == 2 && checkMethod(insn.getPrevious(), "posToState") && checkMethod(insn, "getPosOpacity")) {
            instructions.remove(getPrevious(insn, 6));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "me/jellysquid/mods/phosphor/mod/world/lighting/LightingEngine", "world", "Lnet/minecraft/world/World;"));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 12));
            instructions.insertBefore(insn, genMethodNode("getNeighborOpacity", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.remove(insn);
            return true;
        }
        /*
         * spreadLightFromCursor:
         * Old code:
         * final int newLight = curLight - this.getPosOpacity(info.pos, LightingEngineHelpers.posToState(info.pos, info.section));
         *
         * New code:
         * //account for FluidState
         * final int newLight = curLight - Hooks.getNeighborOpacity(info.pos, LightingEngineHelpers.posToState(info.pos, info.section), this.world, nChunk);
         */
        else if(index == 3 && checkMethod(insn.getPrevious(), "posToState") && checkMethod(insn, "getPosOpacity")) {
            instructions.remove(getPrevious(insn, 8));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "me/jellysquid/mods/phosphor/mod/world/lighting/LightingEngine", "world", "Lnet/minecraft/world/World;"));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 7));
            instructions.insertBefore(insn, genMethodNode("getNeighborOpacity", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.remove(insn);
            return true;
        }
        /*
         * getCursorLuminosity:
         * Old code:
         * return MathHelper.clamp(LightingEngineHelpers.getLightValueForState(state, this.world, this.curPos), 0, MAX_LIGHT);
         *
         * New code:
         * //account for FluidState
         * return MathHelper.clamp(Hooks.compareWithFluid(LightingEngineHelpers.getLightValueForState(state, this.world, this.curPos), this.world, this.curPos, this.curChunk), 0, MAX_LIGHT);
         */
        else if(index == 4 && checkMethod(insn, "getLightValueForState")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "me/jellysquid/mods/phosphor/mod/world/lighting/LightingEngine", "world", "Lnet/minecraft/world/World;"));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "me/jellysquid/mods/phosphor/mod/world/lighting/LightingEngine", "curPos", "Lnet/minecraft/util/math/BlockPos$MutableBlockPos;"));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "me/jellysquid/mods/phosphor/mod/world/lighting/LightingEngine", "curChunk", "Lnet/minecraft/world/chunk/Chunk;"));
            list.add(genMethodNode("compareWithFluid", "(ILnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.insert(insn, list);
            return true;
        }
        /*
         * getPosOpacity:
         * Old code:
         * return MathHelper.clamp(state.getLightOpacity(this.world, pos), 1, MAX_LIGHT);
         *
         * New code:
         * //account for FluidState
         * return MathHelper.clamp(Hooks.getLightOpacity(state, this.world, pos, this.curChunk), 1, MAX_LIGHT);
         */
        else if(index == 5 && checkMethod(insn, "getLightOpacity")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "me/jellysquid/mods/phosphor/mod/world/lighting/LightingEngine", "curChunk", "Lnet/minecraft/world/chunk/Chunk;"));
            instructions.insertBefore(insn, genMethodNode("getLightOpacity", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static int compareWithFluid(int light, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return Math.max(light, FluidState.getFromProvider(chunk, pos).getState().getLightValue(world, pos));
        }

        public static int getLightOpacity(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return PluginChunk.Hooks.getFluidLightOpacity(state, world, pos, chunk);
        }

        public static int getNeighborOpacity(@Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull World world, @Nonnull Chunk chunk) {
            return MathHelper.clamp(getLightOpacity(state, world, pos, chunk), 1, 15);
        }
    }
}
