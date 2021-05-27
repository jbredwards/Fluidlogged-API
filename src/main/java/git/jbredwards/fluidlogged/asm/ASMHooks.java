package git.jbredwards.fluidlogged.asm;

import com.google.common.collect.Lists;
import git.jbredwards.fluidlogged.common.block.AbstractFluidloggedBlock;
import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.common.block.IParticleColor;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import git.jbredwards.fluidlogged.util.FluidloggedConstants;
import git.jbredwards.fluidlogged.util.FluidloggedUtils;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BlockModelShapes;
import net.minecraft.client.renderer.BlockRendererDispatcher;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.chunk.ChunkCompileTaskGenerator;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.client.renderer.color.BlockColors;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.init.Blocks;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.TextComponentString;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.client.ForgeHooksClient;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.BlockFluidClassic;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.Logger;

import java.util.Objects;
import java.util.Queue;

import static git.jbredwards.fluidlogged.asm.OFReflector.*;
import static net.minecraft.block.BlockStairs.FACING;
import static net.minecraft.block.BlockStairs.HALF;

/**
 * exists cause SpongeForge
 * @author jbred
 *
 */
@SuppressWarnings("unused")
public enum ASMHooks
{
    ;

    //BlockFenceGatePlugin
    public static boolean setStoredOrRealSimple(World world, BlockPos pos, IBlockState state, int flags) {
        final TileEntity te = world.getTileEntity(pos);

        //fluidlogged
        if(te instanceof TileEntityFluidlogged) {
            ((TileEntityFluidlogged)te).setStored(state, true);
            return true;
        }

        //default
        return world.setBlockState(pos, state, flags);
    }

    //BlockFluidClassicPlugin
    public static int getQuantaValue(BlockFluidClassic fluid, IBlockState state, int quantaPerBlock) {
        //modded
        if(state.getBlock() instanceof BlockFluidBase && fluid.getFluid().getBlock() instanceof BlockFluidBase) {
            final int level = state.getValue(BlockFluidBase.LEVEL);
            return (((BlockFluidBase)state.getBlock()).getFluid().getBlock() == fluid.getFluid().getBlock()) ? (quantaPerBlock - level) : -1;
        }
        //vanilla
        else if(state.getBlock() instanceof BlockLiquid && fluid.getFluid().getBlock() instanceof BlockLiquid) {
            final int level = state.getValue(BlockLiquid.LEVEL);
            return (state.getMaterial() == fluid.getDefaultState().getMaterial()) ? (quantaPerBlock - level) : -1;
        }
        //default
        else return -1;
    }

    //BlockModelShapesPlugin
    @SideOnly(Side.CLIENT)
    public static void registerBuiltinBlocks(BlockModelShapes shapes) {
        for(BlockFluidloggedTE block : FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.values()) {
            //makes sure to not register the vanilla ones
            if(block.fluid != FluidRegistry.WATER && block.fluid != FluidRegistry.LAVA) {
                shapes.registerBuiltInBlocks(block);
            }
        }
    }

    //BlockSpongePlugin
    public static boolean absorb(BlockSponge sponge, World world, BlockPos posIn) {
        final Queue<Pair<BlockPos, Integer>> queue = Lists.newLinkedList();
        queue.add(Pair.of(posIn, 0));

        int blocksDrained = 0;

        while(!queue.isEmpty()) {
            Pair<BlockPos, Integer> pair = queue.poll();
            BlockPos pos = pair.getLeft();
            int distance = pair.getRight();

            for(EnumFacing facing : EnumFacing.values()) {
                BlockPos offset = pos.offset(facing);
                IBlockState state = world.getBlockState(offset);

                if(state.getMaterial() == Material.WATER) {
                    //custom drain action
                    if(state.getBlock() instanceof AbstractFluidloggedBlock) ((AbstractFluidloggedBlock)state.getBlock()).drain(world, offset, true);
                    else world.setBlockState(offset, Blocks.AIR.getDefaultState());

                    ++blocksDrained;

                    if(distance < 6) queue.add(Pair.of(offset, distance + 1));
                }
            }

            if(blocksDrained > 64) break;
        }

        return blocksDrained > 0;
    }

    //BlockStairsPlugin
    public static BlockStairs.EnumShape getShape(IBlockState state, IBlockAccess world, BlockPos pos) {
        final EnumFacing face = state.getValue(FACING);
        IBlockState neighbor = FluidloggedUtils.getStoredOrReal(world, pos.offset(face));
        //if outer shape
        if(neighbor.getBlock() instanceof BlockStairs && state.getValue(HALF) == neighbor.getValue(HALF)) {
            final EnumFacing neighborFace = neighbor.getValue(FACING);

            if(neighborFace.getAxis() != face.getAxis() && isDifferentStairs(state, world, pos, neighborFace.getOpposite())) {
                if(neighborFace == face.rotateYCCW()) return BlockStairs.EnumShape.OUTER_LEFT;
                else return BlockStairs.EnumShape.OUTER_RIGHT;
            }
        }
        //if inner shape
        neighbor = FluidloggedUtils.getStoredOrReal(world, pos.offset(face.getOpposite()));
        if(neighbor.getBlock() instanceof BlockStairs && state.getValue(HALF) == neighbor.getValue(HALF)) {
            final EnumFacing neighborFace = neighbor.getValue(FACING);

            if(neighborFace.getAxis() != face.getAxis() && isDifferentStairs(state, world, pos, neighborFace)) {
                if(neighborFace == face.rotateYCCW()) return BlockStairs.EnumShape.INNER_LEFT;
                else return BlockStairs.EnumShape.INNER_RIGHT;
            }
        }

        //default return statement
        return BlockStairs.EnumShape.STRAIGHT;
    }

    //BlockStairsPlugin
    public static boolean isDifferentStairs(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing offset) {
        final IBlockState neighbor = FluidloggedUtils.getStoredOrReal(world, pos.offset(offset));
        return !(neighbor.getBlock() instanceof BlockStairs) || state.getValue(FACING) != neighbor.getValue(FACING) || state.getValue(HALF) != neighbor.getValue(HALF);
    }

    //FluidPlugin
    public static void registerFluidloggedBlock(Fluid fluid) {
        if(fluid.getName() != null && FluidRegistry.isFluidRegistered(fluid) && fluid.getBlock() instanceof BlockFluidClassic && !FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.containsKey(fluid)) {
            //generates the block
            final BlockFluidloggedTE block = new BlockFluidloggedTE(fluid, fluid.getBlock().getDefaultState().getMaterial());
            //stores the block
            FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.put(fluid, block);

            //registers the block
            ForgeRegistries.BLOCKS.register(block.setRegistryName(fluid.getName() + "logged_te"));
        }
    }

    //FluidPlugin
    public static void fluidBlockErrorSpamFix(Logger logger, String message, Block block, String fluidName, Block old) {}

    //ParticleDiggingPlugin
    @SideOnly(Side.CLIENT)
    public static int getColor(BlockColors old, IBlockState state, World world, BlockPos pos, int index) {
        if(state.getBlock() instanceof IParticleColor) return ((IParticleColor)state.getBlock()).getParticleColor(state, world, pos);
        else return old.colorMultiplier(state, world, pos, index);
    }

    //RenderChunkPlugin
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

    //RenderChunkPlugin
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
