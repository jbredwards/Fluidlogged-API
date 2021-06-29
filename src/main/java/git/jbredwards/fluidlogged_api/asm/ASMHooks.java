package git.jbredwards.fluidlogged_api.asm;

import com.google.common.collect.Lists;
import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.util.FluidloggedConstants;
import git.jbredwards.fluidlogged_api.common.block.AbstractFluidloggedBlock;
import git.jbredwards.fluidlogged_api.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged_api.common.block.IParticleColor;
import git.jbredwards.fluidlogged_api.common.block.TileEntityFluidlogged;
import git.jbredwards.fluidlogged_api.util.FluidloggedUtils;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
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
import net.minecraft.util.ResourceLocation;
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

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

import static net.minecraft.block.BlockStairs.FACING;
import static net.minecraft.block.BlockStairs.HALF;
import static net.minecraft.util.EnumFacing.*;

/**
 * class exists cause SpongeForge
 * NOTE THAT MOST OF THESE METHODS ARE MEANT TO ONLY BE USED IN CERTAIN CASES,
 * PRIOR TO INTEGRATING THEM TO YOUR OWN MOD, VIEW THE PLUGIN CLASS ASSOCIATED
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

    //BlockFluidClassicPlugin
    public static Block isSourceBlock(Block block, BlockFluidClassic obj, IBlockAccess world, BlockPos pos) {
        final boolean flag = FluidloggedUtils.getFluidFromBlock(obj) == FluidloggedUtils.getFluidFromBlock(block);
        return flag ? obj : null;
    }

    //BlockFluidClassicPlugin
    public static int getLargerQuanta(BlockFluidClassic obj, World world, BlockPos pos, int compare, EnumFacing facing, Fluid fluidIn, int quantaPerBlock, int densityDir) {
        final boolean hasVerticalFlow = hasVerticalFlow(world, pos, densityDir, obj) == obj;
        final int quantaValue = obj.getQuantaValue(world, pos);
        int quantaRemaining = (quantaValue > 0 && quantaValue < quantaPerBlock && hasVerticalFlow ? quantaPerBlock : quantaValue);
        if(quantaRemaining <= 0) return compare;

        //new stuff
        final IBlockState state = world.getBlockState(pos);
        if(FluidloggedUtils.getFluidFromBlock(state.getBlock()) == fluidIn) {
            if(state.getBlock() instanceof AbstractFluidloggedBlock && !((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, pos, facing.getOpposite())) return compare;
        }

        return Math.max(quantaRemaining, compare);
    }

    //BlockModelShapesPlugin
    @SideOnly(Side.CLIENT)
    public static void registerBuiltinBlocks(BlockModelShapes shapes) {
        shapes.registerBuiltInBlocks(FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.values().toArray(new BlockFluidloggedTE[0]));
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
            final BlockFluidloggedTE block = new BlockFluidloggedTE(fluid, fluid.getBlock().getDefaultState().getMaterial(), fluid.getBlock().getDefaultState().getMapColor(null, null));
            //registers the block
            FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.put(fluid, block);
            ForgeRegistries.BLOCKS.register(block.setRegistryName(new ResourceLocation(fluid.getName()).getResourcePath() + "logged_te").setUnlocalizedName(fluid.getUnlocalizedName()));
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
        OFReflector.load();

        if(block instanceof BlockFluidloggedTE) {
            final BlockRendererDispatcher renderer = Minecraft.getMinecraft().getBlockRendererDispatcher();
            IBlockState stored = Objects.requireNonNull(FluidloggedUtils.getStored(chunkCacheOF, pos)).getActualState(chunkCacheOF, pos);

            if(stored.getRenderType() != EnumBlockRenderType.INVISIBLE) {
                for(BlockRenderLayer layer : BlockRenderLayer.values()) {
                    if(stored.getBlock().canRenderInLayer(stored, layer)) {
                        try {
                            ForgeHooksClient.setRenderLayer(layer);

                            BufferBuilder buffer = generator.getRegionRenderCacheBuilder().getWorldRendererByLayer(layer);
                            Objects.requireNonNull(OFReflector.setBlockLayer).invoke(buffer, layer);

                            Object renderEnv = Objects.requireNonNull(OFReflector.getRenderEnv).invoke(buffer, stored, pos);
                            Objects.requireNonNull(OFReflector.setRegionRenderCacheBuilder).invoke(renderEnv, generator.getRegionRenderCacheBuilder());

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
                            if((boolean)Objects.requireNonNull(OFReflector.isOverlaysRendered).invoke(renderEnv)) {
                                Objects.requireNonNull(OFReflector.postRenderOverlays).invoke(renderChunk, generator.getRegionRenderCacheBuilder(), compiledChunk, array);
                                Objects.requireNonNull(OFReflector.setOverlaysRendered).invoke(renderEnv, false);
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

    //BlockLiquidPlugin
    public static boolean getFlow(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing facing, BlockLiquid obj) {
        final Fluid fluidIn = FluidloggedUtils.getFluidFromBlock(obj);
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(state.getBlock());
        //fluidlogged block
        if(fluid == fluidIn && state.getBlock() instanceof AbstractFluidloggedBlock) {
            if(!((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, pos, facing.getOpposite())) return true;
        }
        //if fluids aren't equal
        if(fluid != null && fluid != fluidIn) return true;
            //default
        else return state.getMaterial().blocksMovement();
    }

    //BlockDynamicLiquidPlugin
    public static int checkBlockHorizontal(BlockDynamicLiquid obj, World world, BlockPos offset, int currentMinLevel, EnumFacing facing) {
        int level = getDepth(obj, world, offset, facing);
        if(level < 0) return currentMinLevel;
        else {
            if(level == 0) ++obj.adjacentSourceBlocks;
            if(level >= 8) level = 0;

            return currentMinLevel >= 0 && level >= currentMinLevel ? currentMinLevel : level;
        }
    }

    //BlockDynamicLiquidPlugin
    public static int getDepth(BlockDynamicLiquid obj, World world, BlockPos offset, EnumFacing facing) {
        final IBlockState state = world.getBlockState(offset);
        final boolean fluidMatches = FluidloggedUtils.getFluidFromBlock(state.getBlock()) == FluidloggedUtils.getFluidFromBlock(obj);
        final boolean canSideFlow = !(state.getBlock() instanceof AbstractFluidloggedBlock) || ((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, offset, facing.getOpposite());
        return fluidMatches && canSideFlow ? state.getValue(BlockLiquid.LEVEL) : -1;
    }

    //BlockDynamicLiquidPlugin
    public static boolean canFlowDown(BlockDynamicLiquid obj, World world, BlockPos offset, IBlockState down, IBlockState here) {
        final boolean isSource = FluidloggedUtils.isSource(world, offset.up(), here);
        final boolean fluidMatches = FluidloggedUtils.getFluidFromBlock(obj) == FluidloggedUtils.getFluidFromBlock(down.getBlock());
        return (!isSource || !fluidMatches) && down.getBlock().isReplaceable(world, offset);
    }

    //BlockDynamicLiquidPlugin
    public static Material checkForMixing(World world, BlockPos pos) {
        final BlockPos offset = pos.down();
        final IBlockState state = world.getBlockState(offset);

        if(state.getMaterial() == Material.WATER) {
            if(!(state.getBlock() instanceof AbstractFluidloggedBlock) || ((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, offset, UP)) return Material.WATER;
        }

        //default
        return Material.ROCK;
    }

    //BlockDynamicLiquidPlugin
    public static Set<EnumFacing> flowInto(BlockDynamicLiquid obj, World world, BlockPos pos, int i, int j) {
        final int[] flowCost = new int[4];
        int level = i + j;
        //has vertical flow
        if(i >= 8) level = 1;
        //cannot flow further
        else if(level < 0 || level >= 8) return new HashSet<>();

        //flows outward in each possible horizontal direction
        for(int index = 0; index < 4; index++) {
            EnumFacing facing = getHorizontal(index);
            BlockPos offset = pos.offset(facing);
            IBlockState state = world.getBlockState(offset);

            flowCost[index] = 1000;

            //side cannot flow
            if(FluidloggedUtils.isSource(world, offset, state) || !state.getBlock().isReplaceable(world, offset)) continue;

            if(world.getBlockState(offset.down(1)).getBlock().isReplaceable(world, offset.down(1))) flowCost[index] = 0;
            else flowCost[index] = calculateFlowCost(obj, world, offset, 1, index);
        }

        //does the flow
        final int min = Ints.min(flowCost);
        for(int index = 0; index < 4; index++) {
            if(flowCost[index] == min) {
                EnumFacing facing = getHorizontal(index);
                BlockPos offset = pos.offset(facing);
                IBlockState state = world.getBlockState(offset);

                if(FluidloggedUtils.getFluidFromBlock(obj) != FluidloggedUtils.getFluidFromBlock(state.getBlock()) && state.getBlock().isReplaceable(world, offset)) {
                    if(state.getBlock() != Blocks.SNOW_LAYER) state.getBlock().dropBlockAsItem(world, offset, state, 0);
                    world.setBlockState(offset, obj.getDefaultState().withProperty(BlockLiquid.LEVEL, level));
                }
            }
        }

        //returns empty set cause that will prevent the old code from doing stuff
        return new HashSet<>();
    }

    //BlockDynamicLiquidPlugin
    public static int calculateFlowCost(BlockDynamicLiquid obj, World world, BlockPos pos, int recurseDepth, int index) {
        final EnumFacing facing = getHorizontal(index);
        int cost = 1000;

        for(int adjIndex = 0; adjIndex < 4; adjIndex++) {
            EnumFacing adjFacing = getHorizontal(adjIndex);
            //won't take the origin into account
            if(adjFacing == facing.getOpposite()) continue;

            BlockPos offset = pos.offset(adjFacing);
            IBlockState state = world.getBlockState(offset);

            //side cannot flow
            if(FluidloggedUtils.isSource(world, offset, state) || !state.getBlock().isReplaceable(world, offset)) continue;

            //flow down
            if(world.getBlockState(offset.down(1)).getBlock().isReplaceable(world, offset.down(1))) return recurseDepth;

            //side cannot flow
            if(recurseDepth >= (obj.getDefaultState().getMaterial() == Material.LAVA && !world.provider.doesWaterVaporize() ? 2 : 4)) continue;

            cost = Math.min(cost, calculateFlowCost(obj, world, offset, recurseDepth + 1, adjIndex));
        }

        return cost;
    }

    //BlockFluidBasePlugin
    public static Block canDisplace(Block block, BlockFluidBase fluid, IBlockAccess world, BlockPos pos) {
        final boolean flag = FluidloggedUtils.getFluidFromBlock(block) == fluid.getFluid();
        return flag || !block.isReplaceable(world, pos) ? fluid : null;
    }

    //BlockFluidBasePlugin
    public static Block hasVerticalFlow(IBlockAccess world, BlockPos pos, int densityDir, BlockFluidBase obj) {
        final BlockPos offset = pos.down(densityDir);
        final IBlockState state = world.getBlockState(offset);

        if(FluidloggedUtils.getFluidFromBlock(state.getBlock()) == obj.getFluid()) {
            final EnumFacing facing = getFacingFromVector(0, densityDir, 0);
            final boolean flag = (!(state.getBlock() instanceof AbstractFluidloggedBlock) || ((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, offset, facing));

            return flag ? obj : null;
        }

        //default (false)
        return null;
    }

    //BlockFluidBasePlugin
    public static float getFluidHeightForRender(BlockFluidBase obj, IBlockAccess world, BlockPos pos, IBlockState up, int i, int j, int densityDir, int quantaPerBlock, float quantaFraction, float quantaPerBlockFloat) {
        final Fluid fluid = obj.getFluid();

        //check block above
        if(FluidloggedUtils.getFluidFromBlock(up.getBlock()) == fluid) {
            if(!(up.getBlock() instanceof AbstractFluidloggedBlock) || ((AbstractFluidloggedBlock)up.getBlock()).canSideFlow(up, world, pos.down(densityDir), densityDir == 1 ? UP : DOWN)) return 1;
        }

        final IBlockState state = world.getBlockState(pos);

        //is air
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final boolean canSideFlow = canSideFlow(fluid, state, world, pos, i, j);
        final boolean fluidMatches = (FluidloggedUtils.getFluidFromBlock(state.getBlock()) == fluid);

        if(fluidMatches && canSideFlow) {
            //max render height
            if(state.getValue(BlockLiquid.LEVEL) == obj.getMaxRenderHeightMeta()) return quantaFraction;
        }

        //not fluid
        if(!fluidMatches || !canSideFlow) return (-1 / quantaPerBlockFloat) * quantaFraction;
        //fluid
        else return ((quantaPerBlock - state.getValue(BlockLiquid.LEVEL)) / quantaPerBlockFloat) * quantaFraction;
    }

    //BlockFluidBasePlugin (corrects side angles)
    @Nonnull public static boolean[] fixN = {false, false};
    @Nonnull public static boolean[] fixS = {false, false};
    @Nonnull public static boolean[] fixE = {false, false};
    @Nonnull public static boolean[] fixW = {false, false};

    //BlockFluidBasePlugin
    public static boolean canSideFlow(Fluid fluid, IBlockState state, IBlockAccess world, BlockPos pos, int i, int j) {
        //SE
        if(i == 0 && j == 0) {
            if(state.getBlock() instanceof AbstractFluidloggedBlock) return canSideFlowDir(fluid, state, world, pos, SOUTH, EAST);
            else return state.getBlockFaceShape(world, pos, SOUTH) != BlockFaceShape.SOLID || state.getBlockFaceShape(world, pos, EAST) != BlockFaceShape.SOLID;
        }
        //S
        else if(i == 1  && j == 0) {
            fixS = new boolean[]{false, false};
            if(state.getBlock() instanceof AbstractFluidloggedBlock) {
                if(canSideFlowDir(fluid, state, world, pos, SOUTH, null)) return true;

                //fix uneven corners
                final boolean flag1 = canSideFlowDir(fluid, state, world, pos, SOUTH, EAST);
                final boolean flag2 = canSideFlowDir(fluid, state, world, pos, SOUTH, WEST);
                if(flag1 != flag2) {
                    if(flag1) fixS[0] = true;
                    else      fixS[1] = true;
                }

                return flag1 || flag2;
            }
            else return state.getBlockFaceShape(world, pos, SOUTH) != BlockFaceShape.SOLID;
        }
        //SW
        else if(i == 2  && j == 0) {
            if(state.getBlock() instanceof AbstractFluidloggedBlock) return canSideFlowDir(fluid, state, world, pos, SOUTH, WEST);
            else return state.getBlockFaceShape(world, pos, SOUTH) != BlockFaceShape.SOLID || state.getBlockFaceShape(world, pos, WEST) != BlockFaceShape.SOLID;
        }
        //E
        else if(i == 0 && j == 1)  {
            fixE = new boolean[]{false, false};
            if(state.getBlock() instanceof AbstractFluidloggedBlock) {
                if(canSideFlowDir(fluid, state, world, pos, EAST, null)) return true;

                //fix uneven corners
                final boolean flag1 = canSideFlowDir(fluid, state, world, pos, EAST, SOUTH);
                final boolean flag2 = canSideFlowDir(fluid, state, world, pos, EAST, NORTH);
                if(flag1 != flag2) {
                    if(flag1) fixE[0] = true;
                    else      fixE[1] = true;
                }

                return flag1 || flag2;
            }
            else return state.getBlockFaceShape(world, pos, EAST) != BlockFaceShape.SOLID;
        }
        //W
        else if(i == 2  && j == 1)  {
            fixW = new boolean[]{false, false};
            if(state.getBlock() instanceof AbstractFluidloggedBlock) {
                if(canSideFlowDir(fluid, state, world, pos, WEST, null)) return true;

                //fix uneven corners
                final boolean flag1 = canSideFlowDir(fluid, state, world, pos, WEST, SOUTH);
                final boolean flag2 = canSideFlowDir(fluid, state, world, pos, WEST, NORTH);
                if(flag1 != flag2) {
                    if(flag1) fixW[0] = true;
                    else      fixW[1] = true;
                }

                return flag1 || flag2;
            }
            else return state.getBlockFaceShape(world, pos, WEST) != BlockFaceShape.SOLID;
        }
        //NE
        else if(i == 0 && j == 2)  {
            if(state.getBlock() instanceof AbstractFluidloggedBlock) return canSideFlowDir(fluid, state, world, pos, NORTH, EAST);
            else return state.getBlockFaceShape(world, pos, NORTH) != BlockFaceShape.SOLID || state.getBlockFaceShape(world, pos, EAST) != BlockFaceShape.SOLID;
        }
        //N
        else if(i == 1  && j == 2)  {
            fixN = new boolean[]{false, false};
            if(state.getBlock() instanceof AbstractFluidloggedBlock) {
                if(canSideFlowDir(fluid, state, world, pos, NORTH, null)) return true;

                //fix uneven corners
                final boolean flag1 = canSideFlowDir(fluid, state, world, pos, NORTH, EAST);
                final boolean flag2 = canSideFlowDir(fluid, state, world, pos, NORTH, WEST);
                if(flag1 != flag2) {
                    if(flag1) fixN[0] = true;
                    else      fixN[1] = true;
                }

                return flag1 || flag2;
            }
            else return state.getBlockFaceShape(world, pos, NORTH) != BlockFaceShape.SOLID;
        }
        //NW
        else if(i == 2  && j == 2)  {
            if(state.getBlock() instanceof AbstractFluidloggedBlock) return canSideFlowDir(fluid, state, world, pos, NORTH, WEST);
            else return state.getBlockFaceShape(world, pos, NORTH) != BlockFaceShape.SOLID || state.getBlockFaceShape(world, pos, WEST) != BlockFaceShape.SOLID;
        }

        //should never pass
        return !state.getMaterial().isSolid();
    }

    //BlockFluidBasePlugin
    public static boolean canSideFlowDir(Fluid fluid, IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing facing1, @Nullable EnumFacing facing2) {
        final AbstractFluidloggedBlock block = (AbstractFluidloggedBlock)state.getBlock();
        if(block.canSideFlow(state, world, pos, facing1) && canFlowInto(fluid, block, world, pos, facing1)) return true;
        else return facing2 != null && block.canSideFlow(state, world, pos, facing2) && canFlowInto(fluid, block, world, pos, facing2);
    }

    //BlockFluidBasePlugin
    public static boolean canFlowInto(Fluid fluid, AbstractFluidloggedBlock block, IBlockAccess world, BlockPos pos, EnumFacing facing) {
        final BlockPos offset = pos.offset(facing);
        final IBlockState state = world.getBlockState(offset);
        final boolean matches = FluidloggedUtils.getFluidFromBlock(state.getBlock()) == fluid;
        final boolean replaceable = state.getBlock().isReplaceable(world, offset);

        return (matches && replaceable) || block.canDisplace(world, pos.offset(facing));
    }

    //BlockFluidBasePlugin
    public static boolean isFluid(IBlockState up, BlockFluidBase obj, IBlockAccess world, int densityDir, BlockPos pos) {
        if(FluidloggedUtils.getFluidFromBlock(up.getBlock()) == obj.getFluid()) {
            return !(up.getBlock() instanceof AbstractFluidloggedBlock) || ((AbstractFluidloggedBlock)up.getBlock()).canSideFlow(up, world, pos.down(densityDir), densityDir == 1 ? UP : DOWN);
        }

        //default
        return false;
    }

    //BlockFluidBasePlugin
    public static Block causesDownwardCurrent(Block block, BlockFluidBase obj, Fluid fluid) {
        return FluidloggedUtils.getFluidFromBlock(block) == fluid ? obj : null;
    }

    //BlockFluidBasePlugin
    public static int getFlowDecay(BlockFluidBase obj, IBlockAccess world, BlockPos pos, @Nullable EnumFacing facing, Fluid fluidIn, int quantaPerBlock, int densityDir) {
        final IBlockState state = world.getBlockState(pos);
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(state.getBlock());
        int quantaValue;

        if(state.getBlock().isAir(state, world, pos)) quantaValue = 0;
        else if(fluid != fluidIn) quantaValue = -1;
        else if(facing != null && state.getBlock() instanceof AbstractFluidloggedBlock && !((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, pos, facing.getOpposite())) quantaValue = -1;
        else quantaValue = quantaPerBlock - state.getValue(BlockLiquid.LEVEL);

        return quantaPerBlock - (quantaValue > 0 && quantaValue < quantaPerBlock && hasVerticalFlow(world, pos, densityDir, obj) == obj ? quantaPerBlock : quantaValue);
    }

    //BlockFluidBasePlugin
    public static boolean getFlowVector(IBlockAccess world, BlockPos pos, EnumFacing facing, Fluid fluidIn) {
        final IBlockState state = world.getBlockState(pos);
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(state.getBlock());
        //fluidlogged block
        if(fluid == fluidIn && state.getBlock() instanceof AbstractFluidloggedBlock) {
            if(!((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, pos, facing.getOpposite())) return true;
        }
        //if fluids aren't equal
        if(fluid != null && fluid != fluidIn) return true;
        //default
        else return state.getMaterial().blocksMovement();
    }

    //BlockFluidBasePlugin
    public static Material shouldSideBeRendered(IBlockState state, Fluid fluid, IBlockAccess world, BlockPos pos, EnumFacing facing) {
        if(FluidloggedUtils.getFluidFromBlock(state.getBlock()) == fluid) {
            if(!(state.getBlock() instanceof AbstractFluidloggedBlock) || ((AbstractFluidloggedBlock)state.getBlock()).canSideFlow(state, world, pos.offset(facing), facing.getOpposite())) return state.getMaterial();
        }

        //default
        return null;
    }

    //BlockFluidRendererPlugin
    public static float getFluidHeightForRender(Fluid fluid, IBlockAccess world, BlockPos pos, IBlockState up, int i, int j) {
        //check block above
        if(isFluid(up, fluid, world, pos)) return 1;

        final IBlockState state = world.getBlockState(pos);

        //is air
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final boolean canSideFlow = canSideFlow(fluid, state, world, pos, i, j);
        final boolean fluidMatches = (FluidloggedUtils.getFluidFromBlock(state.getBlock()) == fluid);

        if(fluidMatches && canSideFlow) {
            //max render height
            if(state.getValue(BlockLiquid.LEVEL) == 0) return 8f / 9;
        }

        //not fluid
        if(!fluidMatches|| !canSideFlow) return (-1 / 8f) * (8f / 9);
        //fluid
        else return ((8 - state.getValue(BlockLiquid.LEVEL)) / 8f) * (8f / 9);
    }

    //BlockFluidRendererPlugin
    public static float getFluidHeightAverage(float quantaFraction, int i, int j, float... flow) {
        float total = 0;
        int count = 0;

        for(int index = 0; index < flow.length; index++) {
            //fixes corners flowing into illegal sides (vanilla 1.13 bug)
            if(fixN[i] && j == 1 && index % 2 == 1) continue;
            if(fixS[i] && j == 0 && index % 2 == 0) continue;
            if(fixE[j] && i == 0 && index < 2) continue;
            if(fixW[j] && i == 1 && index > 1) continue;

            //old
            if(flow[index] >= quantaFraction) {
                total += flow[index] * 10;
                count += 10;
            }

            if(flow[index] >= 0) {
                total += flow[index];
                count++;
            }
        }

        return total / count;
    }

    //BlockFluidRendererPlugin
    public static boolean isFluid(IBlockState up, Fluid fluid, IBlockAccess world, BlockPos pos) {
        if(FluidloggedUtils.getFluidFromBlock(up.getBlock()) == fluid) {
            return !(up.getBlock() instanceof AbstractFluidloggedBlock) || ((AbstractFluidloggedBlock)up.getBlock()).canSideFlow(up, world, pos.up(), DOWN);
        }

        //default
        return false;
    }

    //ModelFluidPlugin
    public static float fixTextureFightingZ(float old, int index) {
        final EnumFacing facing = EnumFacing.getHorizontal((5 - index) % 4); // [W, S, E, N]
        if(facing.getAxis() == Axis.X) return old;
        else return old == 1 ? 0.998f : 0.002f;
    }

    //ModelFluidPlugin
    public static float fixTextureFightingX(float old, int index) {
        final EnumFacing facing = EnumFacing.getHorizontal((5 - index) % 4); // [W, S, E, N]
        if(facing.getAxis() == Axis.Z) return old;
        else return old == 1 ? 0.998f : 0.002f;
    }

    //==========
    //MOD COMPAT
    //==========
    public static Block BOPCompat(Block block, Block obj, Fluid fluid) {
        return FluidloggedUtils.getFluidFromBlock(block) == fluid ? obj : null;
    }
}
