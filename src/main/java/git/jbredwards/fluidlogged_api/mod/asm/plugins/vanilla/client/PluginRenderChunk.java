package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.chunk.ChunkCompileTaskGenerator;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.client.ForgeHooksClient;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * allows the game to render FluidStates
 * @author jbred
 *
 */
public final class PluginRenderChunk implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, obfuscated ? "func_178581_b" : "rebuildChunk", null);
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        final boolean isBetterFoliage = checkMethod(insn, "canRenderBlockInLayer", null);

        //VANILLA
        /*
         * rebuildChunk: (changes are around line 189)
         * Old code:
         * if(!block.canRenderInLayer(iblockstate, blockrenderlayer1)) continue;
         *
         * New code:
         * //render FluidState
         * if(!Hooks.renderChunk(block, iblockstate, blockrenderlayer1, aboolean, generator, compiledchunk, this.worldView, blockpos$mutableblockpos, blockpos)) continue;
         */
        if(checkMethod(insn, "canRenderInLayer", null) || (isBetterFoliage && ((VarInsnNode)getPrevious(insn, 2)).var == 15)) {
            final InsnList list = new InsnList();
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 11));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //world variable
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/client/renderer/chunk/RenderChunk", (obfuscated ? "field_189564_r" : "worldView"), "Lnet/minecraft/world/ChunkCache;"));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 14));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //adds the new code
            list.add(genMethodNode("renderChunk", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/BlockRenderLayer;[ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        //OPTIFINE (with Better Foliage)
        /*
         * rebuildChunk:
         * Old code:
         * net.optifine.reflect.Reflector.callBoolean(block, net.optifine.reflect.Reflector.ForgeBlock_canRenderInLayer, iblockstate, blockrenderlayer1);
         *
         * New code:
         * //render FluidState
         *
         */
        else if(isBetterFoliage && ((VarInsnNode)getPrevious(insn, 2)).var == 18) {
            final InsnList list = new InsnList();
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 12));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //chunkCacheOF variable
            list.add(new VarInsnNode(ALOAD, 11));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 17));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //adds the new code
            list.add(genMethodNode("renderChunk", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/BlockRenderLayer;[ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        //OPTIFINE (without Better Foliage)
        else if(checkMethod(insn, "callBoolean", null)) {
            final InsnList list = new InsnList();
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 12));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //chunkCacheOF variable
            list.add(new VarInsnNode(ALOAD, 11));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 17));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //adds the new code
            list.add(genMethodNode("renderChunkOF", "(Ljava/lang/Object;Ljava/lang/Object;[Ljava/lang/Object;[ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean renderChunk(Block block, IBlockState state, BlockRenderLayer layerIn, boolean[] array, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, IBlockAccess world, BlockPos pos, BlockPos chunkPos) {
            //only run fluid renderer once
            if(layerIn.ordinal() == 0 && !FluidloggedUtils.isFluid(state)) {
                final FluidState fluidState = FluidState.get(pos);
                if(!fluidState.isEmpty() && fluidState.getState().getRenderType() == EnumBlockRenderType.MODEL
                        && (!(state.getBlock() instanceof IFluidloggable) || ((IFluidloggable)state.getBlock())
                                .shouldFluidRender(world, pos, state, fluidState))) {

                    //renders the fluid in each layer
                    for(BlockRenderLayer layer : BlockRenderLayer.values()) {
                        if(!fluidState.getBlock().canRenderInLayer(fluidState.getState(), layer))
                            continue;

                        ForgeHooksClient.setRenderLayer(layer);
                        BufferBuilder buffer = generator.getRegionRenderCacheBuilder().getWorldRendererByLayer(layer);

                        if(!compiledChunk.isLayerStarted(layer)) {
                            compiledChunk.setLayerStarted(layer);
                            buffer.begin(7, DefaultVertexFormats.BLOCK);
                            buffer.setTranslation(-chunkPos.getX(), -chunkPos.getY(), -chunkPos.getZ());
                        }

                        //render the fluid
                        array[layer.ordinal()] |= Minecraft.getMinecraft().getBlockRendererDispatcher()
                                .renderBlock(fluidState.getState(), pos, world, buffer);
                    }

                    //reset current render layer
                    ForgeHooksClient.setRenderLayer(null);
                }
            }

            //always return old code
            return canRenderBlockInLayer(block, state, layerIn);
        }

        public static boolean renderChunkOF(Object blockIn, Object ignored, Object[] args, boolean[] array, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, IBlockAccess world, BlockPos pos, BlockPos chunkPos) {
            return renderChunk((Block)blockIn, (IBlockState)args[0], (BlockRenderLayer)args[1], array, generator, compiledChunk, world, pos, chunkPos);
        }

        //helper
        public static boolean canRenderBlockInLayer(@Nonnull Block block, @Nonnull IBlockState state, @Nonnull BlockRenderLayer layer) {
            return FluidloggedAPI.isBetterFoliage ? BFHooks.canRenderBlockInLayer(block, state, layer) : block.canRenderInLayer(state, layer);
        }
    }

    //hold Better Foliage methods in separate class to avoid crash
    public static final class BFHooks
    {
        public static boolean canRenderBlockInLayer(@Nonnull Block block, @Nonnull IBlockState state, @Nonnull BlockRenderLayer layer) {
            return mods.betterfoliage.client.Hooks.canRenderBlockInLayer(block, state, layer);
        }
    }
}
