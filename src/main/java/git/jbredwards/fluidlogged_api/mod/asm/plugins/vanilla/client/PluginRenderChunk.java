package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.chunk.ChunkCompileTaskGenerator;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.util.BlockRenderLayer;
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
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_178581_b" : "rebuildChunk"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //VANILLA
        /*
         * rebuildChunk: (changes are around line 188)
         * Old code:
         * for(BlockRenderLayer blockrenderlayer1 : BlockRenderLayer.values())
         * {
         *     ...
         * }
         *
         * New code:
         * //render FluidState
         * Hooks.renderFluidState(iblockstate, aboolean, generator, compiledchunk, this.worldView, blockpos$mutableblockpos, blockpos);
         * for(BlockRenderLayer blockrenderlayer1 : BlockRenderLayer.values())
         * {
         *     ...
         * }
         */
        if(checkMethod(insn, "values", "()[Lnet/minecraft/util/BlockRenderLayer;") && insn.getPrevious() instanceof FrameNode) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 15));
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
            list.add(genMethodNode("renderFluidState", "(Lnet/minecraft/block/state/IBlockState;[ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V"));
            instructions.insertBefore(insn, list);
            return true;
        }
        //OPTIFINE
        /*
         * rebuildChunk: (changes are around line 188)
         * Old code:
         * for (int ix = 0; ix < blockLayers.length; ix++)
         * {
         *     ...
         * }
         *
         * New code:
         * //render FluidState
         * Hooks.renderFluidState(iblockstate, aboolean, generator, compiledchunk, iblockaccess, blockpos$mutableblockpos, blockpos);
         * for (int ix = 0; ix < blockLayers.length; ix++)
         * {
         *     ...
         * }
         */
        else if(insn instanceof FrameNode && insn.getNext().getOpcode() == ICONST_0) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 18));
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
            list.add(genMethodNode("renderFluidState", "(Lnet/minecraft/block/state/IBlockState;[ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V"));
            instructions.insert(insn, list);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void renderFluidState(@Nonnull IBlockState state, @Nonnull boolean[] array, @Nonnull ChunkCompileTaskGenerator generator, @Nonnull CompiledChunk compiledChunk, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull BlockPos chunkPos) {
            final FluidState fluidState = FluidState.get(world, pos);
            if(!fluidState.isEmpty() && (!(state.getBlock() instanceof IFluidloggable) || ((IFluidloggable)state.getBlock()).shouldFluidRender(world, pos, state, fluidState))) {
                //renders the fluid in each layer
                for(BlockRenderLayer layer : BlockRenderLayer.values()) {
                    if(FluidloggedAPI.isBetterFoliage
                            ? !BFHooks.canRenderBlockInLayer(fluidState.getState(), layer)
                            : !fluidState.getBlock().canRenderInLayer(fluidState.getState(), layer)) continue;

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
    }

    //hold Better Foliage methods in separate class to avoid crash
    public static final class BFHooks
    {
        public static boolean canRenderBlockInLayer(@Nonnull IBlockState state, @Nonnull BlockRenderLayer layer) {
            return mods.betterfoliage.client.Hooks.canRenderBlockInLayer(state.getBlock(), state, layer);
        }
    }
}
