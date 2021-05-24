package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockWall;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BlockRendererDispatcher;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.chunk.ChunkCompileTaskGenerator;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.init.Blocks;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.TextComponentString;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.client.ForgeHooksClient;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Objects;

import static git.jbredwards.fluidlogged.asm.OFReflector.*;
import static org.objectweb.asm.Opcodes.*;

/**
 *
 * @author jbred
 *
 */
public final class RenderChunkPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_178581_b" : "rebuildChunk";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(FFFLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;)V";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //OPTIFINE
        if(insn.getOpcode() == INVOKESTATIC && insn instanceof MethodInsnNode && ((MethodInsnNode)insn).name.equals("callVoid")) {
            if(insn.getPrevious().getPrevious().getOpcode() == ACONST_NULL) {
                final InsnList list = new InsnList();
                list.add(new LabelNode());
                //boolean array variable
                list.add(new VarInsnNode(ALOAD, 12));
                //render chunk variable
                list.add(new VarInsnNode(ALOAD, 0));
                //chunk compiler variable
                list.add(new VarInsnNode(ALOAD, 4));
                //compiled chunk variable
                list.add(new VarInsnNode(ALOAD, 5));
                //block variable
                list.add(new VarInsnNode(ALOAD, 19));
                //chunkCacheOF variable
                list.add(new VarInsnNode(ALOAD, 11));
                //block position variable
                list.add(new VarInsnNode(ALOAD, 17));
                //chunk position variable
                list.add(new VarInsnNode(ALOAD, 7));
                //actually adds the new code
                list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/RenderChunkPlugin", "renderFluidloggedBlockOF", "([ZLnet/minecraft/client/renderer/chunk/RenderChunk;Lnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V", false));

                instructions.insert(insn, list);
                return true;
            }
        }
        //VANILLA
        if(insn.getOpcode() == INVOKESTATIC && insn.getPrevious().getOpcode() == ACONST_NULL) {
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //boolean array variable
            list.add(new VarInsnNode(ALOAD, 11));
            //chunk compiler variable
            list.add(new VarInsnNode(ALOAD, 4));
            //compiled chunk variable
            list.add(new VarInsnNode(ALOAD, 5));
            //block variable
            list.add(new VarInsnNode(ALOAD, 16));
            //world variable
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/client/renderer/chunk/RenderChunk", (obfuscated ? "field_189564_r" : "worldView"), "Lnet/minecraft/world/ChunkCache;"));
            //block position variable
            list.add(new VarInsnNode(ALOAD, 14));
            //chunk position variable
            list.add(new VarInsnNode(ALOAD, 7));
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/RenderChunkPlugin", "renderFluidloggedBlock", "([ZLnet/minecraft/client/renderer/chunk/ChunkCompileTaskGenerator;Lnet/minecraft/client/renderer/chunk/CompiledChunk;Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V", false));

            instructions.insert(insn, list);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    public static void renderFluidloggedBlock(boolean[] array, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, Block block, IBlockAccess world, BlockPos pos, BlockPos chunkPos) {
        if(block instanceof BlockFluidloggedTE) {
            final BlockRendererDispatcher renderer = Minecraft.getMinecraft().getBlockRendererDispatcher();
            IBlockState stored = Objects.requireNonNull(FluidloggedUtils.getStored(world, pos)).getActualState(world, pos);

            if(stored.getRenderType() != EnumBlockRenderType.INVISIBLE) {
                for(BlockRenderLayer layer : BlockRenderLayer.values()) {
                    if(stored.getBlock().canRenderInLayer(stored, layer)) {
                        ForgeHooksClient.setRenderLayer(layer);
                        BufferBuilder buffer = generator.getRegionRenderCacheBuilder().getWorldRendererByLayer(layer);

                        if(!compiledChunk.isLayerStarted(layer)) {
                            compiledChunk.setLayerStarted(layer);
                            buffer.begin(7, DefaultVertexFormats.BLOCK);
                            buffer.setTranslation(-chunkPos.getX(), -chunkPos.getY(), -chunkPos.getZ());
                        }

                        IBakedModel model = renderer.getModelForState(stored);
                        stored = stored.getBlock().getExtendedState(stored, world, pos);

                        //fixes issue with underwater walls
                        if(stored.getBlock() instanceof BlockWall && stored.getValue(BlockWall.UP)) {
                            boolean north = stored.getValue(BlockWall.NORTH);
                            boolean south = stored.getValue(BlockWall.SOUTH);
                            boolean east = stored.getValue(BlockWall.EAST);
                            boolean west = stored.getValue(BlockWall.WEST);

                            if((north && south && !east && !west) || (!north && !south && east && west)) {
                                IBlockState up = FluidloggedUtils.getStoredOrReal(world, pos.up());
                                stored = stored.withProperty(BlockWall.UP, !(up.getMaterial().isLiquid() || up.getBlock() == Blocks.AIR));
                            }
                        }

                        array[layer.ordinal()] |= renderer.getBlockModelRenderer().renderModel(world, model, stored, pos, buffer, true);
                    }
                }

                ForgeHooksClient.setRenderLayer(null);
            }
        }
    }

    //same function as above method, but adapted for optifine compat
    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    public static void renderFluidloggedBlockOF(boolean[] array, RenderChunk renderChunk, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, Block block, IBlockAccess chunkCacheOF, BlockPos pos, BlockPos chunkPos) {
        load();

        if(block instanceof BlockFluidloggedTE) {
            final BlockRendererDispatcher renderer = Minecraft.getMinecraft().getBlockRendererDispatcher();
            IBlockState stored = Objects.requireNonNull(FluidloggedUtils.getStored(chunkCacheOF, pos)).getActualState(chunkCacheOF, pos);

            if(stored.getRenderType() != EnumBlockRenderType.INVISIBLE) {
                for(BlockRenderLayer layer : BlockRenderLayer.values()) {
                    if(stored.getBlock().canRenderInLayer(stored, layer)) {
                        try {
                            ForgeHooksClient.setRenderLayer(layer);

                            BufferBuilder buffer = generator.getRegionRenderCacheBuilder().getWorldRendererByLayer(layer);
                            Objects.requireNonNull(setBlockLayer).invoke(buffer, layer);

                            Object renderEnv = Objects.requireNonNull(getRenderEnv).invoke(buffer, stored, pos);
                            Objects.requireNonNull(setRegionRenderCacheBuilder).invoke(renderEnv, generator.getRegionRenderCacheBuilder());

                            if(!compiledChunk.isLayerStarted(layer)) {
                                compiledChunk.setLayerStarted(layer);
                                buffer.begin(7, DefaultVertexFormats.BLOCK);
                                buffer.setTranslation(-chunkPos.getX(), -chunkPos.getY(), -chunkPos.getZ());
                            }

                            IBakedModel model = renderer.getModelForState(stored);
                            stored = stored.getBlock().getExtendedState(stored, chunkCacheOF, pos);

                            //fixes issue with underwater walls
                            if(stored.getBlock() instanceof BlockWall && stored.getValue(BlockWall.UP)) {
                                boolean north = stored.getValue(BlockWall.NORTH);
                                boolean south = stored.getValue(BlockWall.SOUTH);
                                boolean east = stored.getValue(BlockWall.EAST);
                                boolean west = stored.getValue(BlockWall.WEST);

                                if((north && south && !east && !west) || (!north && !south && east && west)) {
                                    IBlockState up = FluidloggedUtils.getStoredOrReal(chunkCacheOF, pos.up());
                                    stored = stored.withProperty(BlockWall.UP, !(up.getMaterial().isLiquid() || up.getBlock() == Blocks.AIR));
                                }
                            }

                            array[layer.ordinal()] |= renderer.getBlockModelRenderer().renderModel(chunkCacheOF, model, stored, pos, buffer, true);

                            //post shader stuff
                            if((boolean)Objects.requireNonNull(isOverlaysRendered).invoke(renderEnv)) {
                                Objects.requireNonNull(postRenderOverlays).invoke(renderChunk, generator.getRegionRenderCacheBuilder(), compiledChunk, array);
                                Objects.requireNonNull(setOverlaysRendered).invoke(renderEnv, false);
                            }
                        }

                        //shouldn't catch, but if it does, do nothing
                        catch(Exception e) {
                            Minecraft.getMinecraft().player.sendMessage(new TextComponentString(TextFormatting.RED + e.toString()));
                        }
                    }
                }

                ForgeHooksClient.setRenderLayer(null);
            }
        }
    }
}
