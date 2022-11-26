package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.WorldServer;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * FluidStates now get ticked
 * @author jbred
 *
 */
public final class PluginWorldServer implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        //updateBlocks
        if(checkMethod(method, obfuscated ? "func_147456_g" : "updateBlocks", null))
            return 1;
        //updateBlockTick
        else if(checkMethod(method, obfuscated ? "func_175654_a" : "updateBlockTick", null))
            return 2;
        //tickUpdates
        else if(checkMethod(method, obfuscated ? "func_72955_a" : "tickUpdates", null))
            return 3;

        else return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * updateBlocks: (changes are around line 500)
         * Old code:
         * this.profiler.endSection();
         *
         * New code:
         * //perform FluidState random ticks
         * Hooks.tickRandomFluid(this, new BlockPos(k1 + j, i2 + extendedblockstorage.getYLocation(), l1 + k), chunk);
         * this.profiler.endSection();
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_76319_b" : "endSection", "()V")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            //new BlockPos instance
            list.add(new TypeInsnNode(NEW, "net/minecraft/util/math/BlockPos"));
            list.add(new InsnNode(DUP));
            list.add(new VarInsnNode(ILOAD, 14));
            list.add(new VarInsnNode(ILOAD, 6));
            list.add(new InsnNode(IADD));
            list.add(new VarInsnNode(ILOAD, 16));
            list.add(new VarInsnNode(ALOAD, 11));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/chunk/storage/ExtendedBlockStorage", obfuscated ? "func_76662_d" : "getYLocation", "()I", false));
            list.add(new InsnNode(IADD));
            list.add(new VarInsnNode(ILOAD, 15));
            list.add(new VarInsnNode(ILOAD, 7));
            list.add(new InsnNode(IADD));
            list.add(new MethodInsnNode(INVOKESPECIAL, "net/minecraft/util/math/BlockPos", "<init>", "(III)V", false));
            list.add(new VarInsnNode(ALOAD, 5));
            list.add(genMethodNode("tickRandomFluid", "(Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)V"));
            //add new code
            instructions.insertBefore(getPrevious(insn, 2), list);
            return true;
        }
        /*
         * updateBlockTick: (changes are around line 571)
         * Old code:
         * IBlockState iblockstate = this.getBlockState(pos);
         *
         * New code:
         * //use the FluidState here if it matches the block queued for an update
         * IBlockState iblockstate = Hooks.getHereOrFluidIfMatch(this, pos, blockIn);
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            final InsnList list = new InsnList();
            //gets the compared block param
            list.add(new VarInsnNode(ALOAD, 2));
            //adds the new code
            list.add(genMethodNode("getHereOrFluidIfMatch", "(Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        /*
         * tickUpdates: (changes are around line 769)
         * Old code:
         * IBlockState iblockstate = this.getBlockState(nextticklistentry1.position);
         *
         * New code:
         * //use the FluidState here if it matches the block queued for an update
         * IBlockState iblockstate = Hooks.getHereOrFluidIfMatch(this, nextticklistentry1.position, nextticklistentry1.getBlock());
         */
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 4));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/NextTickListEntry", obfuscated ? "func_151351_a" : "getBlock", "()Lnet/minecraft/block/Block;", false));
            list.add(genMethodNode("getHereOrFluidIfMatch", "(Lnet/minecraft/world/WorldServer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void tickRandomFluid(@Nonnull WorldServer world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
            if(fluidState.getBlock().getTickRandomly())
                fluidState.getBlock().randomTick(world, pos, fluidState.getState(), world.rand);
        }

        @Nonnull
        public static IBlockState getHereOrFluidIfMatch(@Nonnull WorldServer world, @Nonnull BlockPos pos, @Nonnull Block compare) {
            if(world.isOutsideBuildHeight(pos)) return Blocks.AIR.getDefaultState();
            final Chunk chunk = world.getChunk(pos);
            final IBlockState here = chunk.getBlockState(pos);
            //actual block
            if(Block.isEqualTo(compare, here.getBlock())) return here;
            //fluid block
            else if(FluidloggedUtils.isFluid(compare)) {
                final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
                if(!fluidState.isEmpty() && Block.isEqualTo(compare, fluidState.getBlock()))
                    return fluidState.getState();
            }

            //default
            return here;
        }
    }
}
