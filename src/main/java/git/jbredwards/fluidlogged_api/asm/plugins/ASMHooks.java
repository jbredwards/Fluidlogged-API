package git.jbredwards.fluidlogged_api.asm.plugins;

import git.jbredwards.fluidlogged_api.asm.replacements.BlockLiquidBase;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggableBase;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedAccessorUtils;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.chunk.ChunkCompileTaskGenerator;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.*;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.client.ForgeHooksClient;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

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

    //=====
    //FORGE
    //=====

    //BlockFluidBasePlugin
    public static Map<Block, Boolean> defaultDisplacements(Map<Block, Boolean> map) {
        final Map<Block, Boolean> ret = new HashMap<>();
        //restore old entries
        ret.put(Blocks.OAK_DOOR,                       false);
        ret.put(Blocks.SPRUCE_DOOR,                    false);
        ret.put(Blocks.BIRCH_DOOR,                     false);
        ret.put(Blocks.JUNGLE_DOOR,                    false);
        ret.put(Blocks.ACACIA_DOOR,                    false);
        ret.put(Blocks.DARK_OAK_DOOR,                  false);
        ret.put(Blocks.TRAPDOOR,                       false);
        ret.put(Blocks.IRON_TRAPDOOR,                  false);
        ret.put(Blocks.OAK_FENCE,                      false);
        ret.put(Blocks.SPRUCE_FENCE,                   false);
        ret.put(Blocks.BIRCH_FENCE,                    false);
        ret.put(Blocks.JUNGLE_FENCE,                   false);
        ret.put(Blocks.DARK_OAK_FENCE,                 false);
        ret.put(Blocks.ACACIA_FENCE,                   false);
        ret.put(Blocks.NETHER_BRICK_FENCE,             false);
        ret.put(Blocks.OAK_FENCE_GATE,                 false);
        ret.put(Blocks.SPRUCE_FENCE_GATE,              false);
        ret.put(Blocks.BIRCH_FENCE_GATE,               false);
        ret.put(Blocks.JUNGLE_FENCE_GATE,              false);
        ret.put(Blocks.DARK_OAK_FENCE_GATE,            false);
        ret.put(Blocks.ACACIA_FENCE_GATE,              false);
        ret.put(Blocks.WOODEN_PRESSURE_PLATE,          false);
        ret.put(Blocks.STONE_PRESSURE_PLATE,           false);
        ret.put(Blocks.LIGHT_WEIGHTED_PRESSURE_PLATE,  false);
        ret.put(Blocks.HEAVY_WEIGHTED_PRESSURE_PLATE,  false);
        ret.put(Blocks.LADDER,                         false);
        ret.put(Blocks.IRON_BARS,                      false);
        ret.put(Blocks.GLASS_PANE,                     false);
        ret.put(Blocks.STAINED_GLASS_PANE,             false);
        ret.put(Blocks.PORTAL,                         false);
        ret.put(Blocks.END_PORTAL,                     false);
        ret.put(Blocks.COBBLESTONE_WALL,               false);
        ret.put(Blocks.BARRIER,                        false);
        ret.put(Blocks.STANDING_BANNER,                false);
        ret.put(Blocks.WALL_BANNER,                    false);
        ret.put(Blocks.CAKE,                           false);
        ret.put(Blocks.IRON_DOOR,                      false);
        ret.put(Blocks.STANDING_SIGN,                  false);
        ret.put(Blocks.WALL_SIGN,                      false);
        ret.put(Blocks.REEDS,                          false);
        //new entries added by other mods (never actually seen mods do this, but just in case)
        ret.putAll(map);

        return ret;
    }

    //BlockFluidBasePlugin (corrects side angles)
    @Nonnull public static boolean[] fixN = new boolean[2];
    @Nonnull public static boolean[] fixS = new boolean[2];
    @Nonnull public static boolean[] fixE = new boolean[2];
    @Nonnull public static boolean[] fixW = new boolean[2];

    //BlockFluidBasePlugin
    public static boolean canSideFlow(Fluid fluid, IBlockState state, IBlockAccess world, BlockPos pos, int i, int j) {
        //SE
        if(i == 0 && j == 0) return canSideFlowDir(fluid, state, world, pos, SOUTH, EAST);
        //S
        else if(i == 1  && j == 0) {
            fixS = new boolean[2];
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
        //SW
        else if(i == 2  && j == 0) return canSideFlowDir(fluid, state, world, pos, SOUTH, WEST);
        //E
        else if(i == 0 && j == 1) {
            fixE = new boolean[2];
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
        //W
        else if(i == 2  && j == 1) {
            fixW = new boolean[2];
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
        //NE
        else if(i == 0 && j == 2) return canSideFlowDir(fluid, state, world, pos, NORTH, EAST);
        //N
        else if(i == 1  && j == 2) {
            fixN = new boolean[2];
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
        //NW
        else if(i == 2  && j == 2) return canSideFlowDir(fluid, state, world, pos, NORTH, WEST);
        //should never pass
        else return !state.getMaterial().isSolid();
    }

    //BlockFluidBasePlugin helper
    public static boolean canSideFlowDir(Fluid fluid, IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing facing1, @Nullable EnumFacing facing2) {
        if(!IFluidloggableBase.canFluidFlow(world, pos, state, fluid, facing1) && canFlowInto(fluid, world, pos.offset(facing1))) return true;
        else return facing2 != null && !IFluidloggableBase.canFluidFlow(world, pos, state, fluid, facing2) && canFlowInto(fluid, world, pos.offset(facing2));
    }

    //BlockFluidBasePlugin helper
    public static boolean canFlowInto(Fluid fluid, IBlockAccess world, BlockPos pos) {
        final Block fluidBlock = fluid.getBlock();

        return fluidBlock instanceof BlockFluidBase ?
                ((BlockFluidBase)fluidBlock).canDisplace(world, pos) :
                BlockLiquidBase.canDisplace(fluidBlock, fluid, world, pos);
    }

    //BlockFluidBasePlugin
    public static Material shouldSideBeRendered(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing side) {
        final BlockPos offset = pos.offset(side);
        final FluidState fluidState = FluidState.get(offset);
        final @Nullable Fluid hereFluid = fluidState.isEmpty() ? FluidloggedUtils.getFluidFromState(state) : fluidState.getFluid();
        final boolean canSideFlow = IFluidloggableBase.canFluidFlow(world, offset, state, hereFluid, side.getOpposite());

        if(fluidState.isEmpty()) return state.getMaterial();
        else return canSideFlow ? fluidState.getState().getMaterial() : null;
    }

    //BlockFluidBasePlugin
    public static Block canDisplace(IBlockState state, Block block, Fluid fluid, IBlockAccess world, BlockPos pos) {
        final boolean flag = FluidloggedUtils.getFluidAt(world, pos, state) == fluid;
        return flag || !isReplaceable(state, world, pos) ? block : null;
    }

    //BlockFluidBasePlugin helper
    public static boolean isReplaceable(IBlockState state, IBlockAccess world, BlockPos pos) {
        return state.getBlock().isReplaceable(world, pos) || (!state.getMaterial().blocksMovement() && !state.getMaterial().isLiquid());
    }

    //FluidPlugin
    public static boolean updateIfNotFluidloggable(Block blockOld, Block blockNew) {
        return blockOld == blockNew && !(blockNew instanceof IFluidloggableBase);
    }

    //FluidPlugin
    public static void fluidBlockErrorSpamFix(Logger logger, String message, Block block, String fluidName, Block old) {
        if(!(block instanceof IFluidloggableBase)) logger.warn(message, block, fluidName, old);
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

    //=======
    //VANILLA
    //=======

    //BlockDynamicLiquidPlugin
    public static boolean placeStaticBlock(World world, BlockPos pos, IBlockState state, int flags) {
        if(isReplaceable(world.getBlockState(pos), world, pos)) return world.setBlockState(pos, state, flags);
        else return false;
    }

    //BlockPlugin
    @SuppressWarnings("deprecation")
    public static float getExplosionResistance(Block block, Entity entity, World world, BlockPos pos, Explosion explosion) {
        final float here = block.getExplosionResistance(entity);
        //catch loop
        if(FluidloggedUtils.getFluidFromBlock(block) != null) return here;
        //no fluid here, return old value
        final FluidState fluidState = FluidState.get(world, pos);
        if(fluidState.isEmpty()) return here;
        //compare the fluid & old values, and return the greater of the two
        else return Math.max(here, fluidState.getBlock().getExplosionResistance(world, pos, entity, explosion));
    }

    //BlockPlugin
    @SuppressWarnings("deprecation")
    public static int getLightOpacity(IBlockState state, IBlockAccess world, BlockPos pos) {
        final int here = state.getLightOpacity();
        //catch loop
        if(FluidloggedUtils.getFluidFromState(state) != null) return here;
        //no fluid here, return old value
        final FluidState fluidState = FluidState.get(world, pos);
        if(fluidState.isEmpty()) return here;
        //compare the fluid & old values, and return the greater of the two
        else return Math.max(here, fluidState.getState().getLightOpacity(world, pos));
    }

    //BlockPlugin
    @SuppressWarnings("deprecation")
    public static int getLightValue(IBlockState state, IBlockAccess world, BlockPos pos) {
        final int here = state.getLightValue();
        //catch loop
        if(FluidloggedUtils.getFluidFromState(state) != null) return here;
        //no fluid here, return old value
        final FluidState fluidState = FluidState.get(world, pos);
        if(fluidState.isEmpty()) return here;
        //compare the fluid & old values, and return the greater of the two
        else return Math.max(here, fluidState.getState().getLightValue(world, pos));
    }

    //RenderChunkPlugin
    public static boolean renderChunk(Block block, IBlockState state, BlockRenderLayer layerIn, boolean[] array, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, IBlockAccess world, BlockPos pos, BlockPos chunkPos) {
        //only run fluid renderer once
        if(layerIn == BlockRenderLayer.SOLID) {
            final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(block);
            final FluidState fluidState = fluid == null ? FluidState.get(pos) :
                    //only render fluid built into here if block instanceof IFluidloggableBase
                    block instanceof IFluidloggableBase ? FluidState.of(fluid) : FluidState.EMPTY;

            if(!fluidState.isEmpty() && IFluidloggableBase.shouldFluidRender(world, pos, state, fluidState.getFluid())) {
                //renders the fluid in each layer
                for(BlockRenderLayer layer : BlockRenderLayer.values()) {
                    if(!fluidState.getBlock().canRenderInLayer(fluidState.getState(), layer))
                        continue;

                    BufferBuilder buffer = generator.getRegionRenderCacheBuilder().getWorldRendererByLayer(layer);
                    ForgeHooksClient.setRenderLayer(layer);

                    if(!compiledChunk.isLayerStarted(layer)) {
                        compiledChunk.setLayerStarted(layer);
                        buffer.begin(7, DefaultVertexFormats.BLOCK);
                        buffer.setTranslation(-chunkPos.getX(), -chunkPos.getY(), -chunkPos.getZ());
                    }

                    //give mods a chance to change something about the rendered fluid
                    IBlockState extendedFluidState = world.getWorldType() != WorldType.DEBUG_ALL_BLOCK_STATES ?
                            fluidState.getState().getActualState(world, pos) : fluidState.getState();

                    //use fluid vertices specific to fluidlogged fluid state
                    extendedFluidState = getExtendedState(extendedFluidState, world, pos, fluidState.getFluid());

                    IBakedModel model = Minecraft.getMinecraft().getBlockRendererDispatcher().getModelForState(extendedFluidState);
                    array[layer.ordinal()] |= Minecraft.getMinecraft().getBlockRendererDispatcher().getBlockModelRenderer()
                            .renderModel(world, model, extendedFluidState, pos, buffer, true);
                }
            }

            //reset current render layer
            ForgeHooksClient.setRenderLayer(null);
        }

        //always return old code
        return block.canRenderInLayer(state, layerIn);
    }

    //RenderChunkPlugin helper
    public static IBlockState getExtendedState(IBlockState oldState, IBlockAccess world, BlockPos pos, Fluid fluid) {
        IExtendedBlockState state = (IExtendedBlockState)oldState.getBlock().getExtendedState(oldState, world, pos);
        final float corner0 = state.getValue(BlockFluidBase.LEVEL_CORNERS[0]);
        final float corner1 = state.getValue(BlockFluidBase.LEVEL_CORNERS[1]);
        final float corner2 = state.getValue(BlockFluidBase.LEVEL_CORNERS[2]);
        final float corner3 = state.getValue(BlockFluidBase.LEVEL_CORNERS[3]);
        //make sure there's not a fluid block above this prior to performing fixes
        if(corner0 != 1 || corner1 != 1 || corner2 != 1 || corner3 != 1) {
            final float quantaFraction = FluidloggedAccessorUtils.quantaFraction(oldState.getBlock());

            if(fixCorner(oldState, world, pos, fluid, NORTH, EAST) || fixCorner(oldState, world, pos, fluid, WEST, SOUTH))
                state = state.withProperty(BlockFluidBase.LEVEL_CORNERS[0], quantaFraction);
            if(fixCorner(oldState, world, pos, fluid, SOUTH, EAST) || fixCorner(oldState, world, pos, fluid, WEST, NORTH))
                state = state.withProperty(BlockFluidBase.LEVEL_CORNERS[1], quantaFraction);
            if(fixCorner(oldState, world, pos, fluid, SOUTH, WEST) || fixCorner(oldState, world, pos, fluid, EAST, NORTH))
                state = state.withProperty(BlockFluidBase.LEVEL_CORNERS[2], quantaFraction);
            if(fixCorner(oldState, world, pos, fluid, NORTH, WEST) || fixCorner(oldState, world, pos, fluid, EAST, SOUTH))
                state = state.withProperty(BlockFluidBase.LEVEL_CORNERS[3], quantaFraction);
        }

        return state;
    }

    //RenderChunkPlugin helper
    public static boolean fixCorner(IBlockState oldState, IBlockAccess world, BlockPos pos, Fluid fluid, EnumFacing primary, EnumFacing other) {
        if(IFluidloggableBase.canFluidFlow(world, pos, oldState, fluid, primary)) return false;

        final BlockPos offset = pos.offset(other);
        final IBlockState neighbor = world.getBlockState(offset);
        final @Nullable Fluid neighborFluid = FluidloggedUtils.getFluidAt(world, offset, neighbor);

        if(neighborFluid != fluid) return true;
        else return !IFluidloggableBase.canFluidFlow(world, offset, neighbor, neighborFluid, primary)
                ||  !IFluidloggableBase.canFluidFlow(world, offset, neighbor, neighborFluid, other);
    }

    //WorldClientPlugin
    public static void showBarrierParticles(WorldClient world, int x, int y, int z, int offset, Random random, BlockPos.MutableBlockPos pos) {
        x += world.rand.nextInt(offset) - world.rand.nextInt(offset);
        y += world.rand.nextInt(offset) - world.rand.nextInt(offset);
        z += world.rand.nextInt(offset) - world.rand.nextInt(offset);

        final FluidState fluidState = FluidState.get(pos.setPos(x, y, z));
        if(!fluidState.isEmpty()) fluidState.getBlock().randomDisplayTick(fluidState.getState(), world, pos, random);
    }

    //WorldPlugin
    public static IBlockState getFluidState(World world, BlockPos pos) {
        final FluidState fluidState = FluidState.get(world, pos);
        return fluidState.isEmpty() ? Blocks.AIR.getDefaultState() : fluidState.getState();
    }

    //WorldPlugin
    public static IBlockState setBlockState(Chunk chunk, BlockPos pos, IBlockState state, IBlockState oldState, World world, int flags) {
        final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
        //replace with empty fluid
        if(!fluidState.isEmpty() && !FluidloggedUtils.isStateFluidloggable(state, fluidState.getFluid()))
            FluidloggedUtils.setFluidState(world, pos, state, FluidState.EMPTY, false, flags);

        //save old state as FluidState
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromState(oldState);
        if(fluid != null && FluidloggedUtils.isFluidFluidloggable(oldState.getBlock()) && oldState.getValue(BlockLiquid.LEVEL) == 0 && FluidloggedUtils.isStateFluidloggable(state, fluid)) {
            FluidloggedUtils.setFluidState(world, pos, state, FluidState.of(fluid), false, flags);
        }

        return chunk.setBlockState(pos, state);
    }

    //WorldServerPlugin
    public static IBlockState updateBlockTick(WorldServer world, BlockPos pos, Block compare) {
        final IBlockState here = world.getBlockState(pos);
        //actual block
        if(Block.isEqualTo(compare, here.getBlock())) return here;
        //fluid
        final FluidState fluidState = FluidState.get(world, pos);
        if(!fluidState.isEmpty() && Block.isEqualTo(compare, fluidState.getBlock())) return fluidState.getState();
        //default
        return here;
    }

    //======
    //MODDED
    //======
}
