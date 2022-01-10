package git.jbredwards.fluidlogged_api.asm.plugins;

import git.jbredwards.fluidlogged_api.asm.replacements.BlockLiquidBase;
import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.AccessorUtils;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.chunk.ChunkCompileTaskGenerator;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.crash.CrashReport;
import net.minecraft.crash.CrashReportCategory;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.init.Items;
import net.minecraft.item.ItemStack;
import net.minecraft.util.*;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.*;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.client.ForgeHooksClient;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.wrappers.FluidBlockWrapper;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

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
        final FluidState fluidState = FluidState.get(world, pos);

        //SE
        if(i == 0 && j == 0) return canSideFlowDir(fluid, state, world, pos, fluidState, SOUTH, EAST);
        //S
        else if(i == 1  && j == 0) {
            fixS = new boolean[2];
            if(canSideFlowDir(fluid, state, world, pos, fluidState, SOUTH)) return true;

            //fix uneven corners
            final boolean flag1 = canSideFlowDir(fluid, state, world, pos, fluidState, SOUTH, EAST);
            final boolean flag2 = canSideFlowDir(fluid, state, world, pos, fluidState, SOUTH, WEST);
            if(flag1 != flag2) {
                if(flag1) fixS[0] = true;
                else      fixS[1] = true;
            }

            return flag1 || flag2;
        }
        //SW
        else if(i == 2  && j == 0) return canSideFlowDir(fluid, state, world, pos, fluidState, SOUTH, WEST);
        //E
        else if(i == 0 && j == 1) {
            fixE = new boolean[2];
            if(canSideFlowDir(fluid, state, world, pos, fluidState, EAST)) return true;

            //fix uneven corners
            final boolean flag1 = canSideFlowDir(fluid, state, world, pos, fluidState, EAST, SOUTH);
            final boolean flag2 = canSideFlowDir(fluid, state, world, pos, fluidState, EAST, NORTH);
            if(flag1 != flag2) {
                if(flag1) fixE[0] = true;
                else      fixE[1] = true;
            }

            return flag1 || flag2;
        }
        //W
        else if(i == 2  && j == 1) {
            fixW = new boolean[2];
            if(canSideFlowDir(fluid, state, world, pos, fluidState, WEST)) return true;

            //fix uneven corners
            final boolean flag1 = canSideFlowDir(fluid, state, world, pos, fluidState, WEST, SOUTH);
            final boolean flag2 = canSideFlowDir(fluid, state, world, pos, fluidState, WEST, NORTH);
            if(flag1 != flag2) {
                if(flag1) fixW[0] = true;
                else      fixW[1] = true;
            }

            return flag1 || flag2;
        }
        //NE
        else if(i == 0 && j == 2) return canSideFlowDir(fluid, state, world, pos, fluidState, NORTH, EAST);
        //N
        else if(i == 1  && j == 2) {
            fixN = new boolean[2];
            if(canSideFlowDir(fluid, state, world, pos, fluidState, NORTH)) return true;

            //fix uneven corners
            final boolean flag1 = canSideFlowDir(fluid, state, world, pos, fluidState, NORTH, EAST);
            final boolean flag2 = canSideFlowDir(fluid, state, world, pos, fluidState, NORTH, WEST);
            if(flag1 != flag2) {
                if(flag1) fixN[0] = true;
                else      fixN[1] = true;
            }

            return flag1 || flag2;
        }
        //NW
        else if(i == 2  && j == 2) return canSideFlowDir(fluid, state, world, pos, fluidState, NORTH, WEST);
        //default
        else return true;
    }

    //BlockFluidBasePlugin helper
    public static boolean canSideFlowDir(Fluid fluid, IBlockState state, IBlockAccess world, BlockPos pos, FluidState fluidState, EnumFacing... sides) {
        for(EnumFacing side : sides) {
            if(FluidloggedUtils.canFluidFlow(world, pos, state, fluid, side) && (fluidState.isEmpty() || canFlowInto(fluid, world, pos.offset(side))))
                return true;
        }

        return false;
    }

    //BlockFluidBasePlugin helper
    public static boolean canFlowInto(Fluid fluid, IBlockAccess world, BlockPos pos) {
        //same fluid or replaceable
        final IBlockState here = world.getBlockState(pos);
        final boolean matches = (FluidloggedUtils.getFluidState(world, pos, here).getFluid() == fluid);
        if(matches && isReplaceable(here, world, pos)) return true;

        //default fallback
        final Block fluidBlock = fluid.getBlock();
        return fluidBlock instanceof BlockFluidBase ?
                ((BlockFluidBase)fluidBlock).canDisplace(world, pos) :
                BlockLiquidBase.canDisplace(fluidBlock, fluid, world, pos);
    }

    //BlockFluidBasePlugin
    public static Block canDisplace(IBlockState state, Block block, Fluid fluid, IBlockAccess world, BlockPos pos) {
        final boolean flag = FluidloggedUtils.getFluidState(world, pos, state).getFluid() == fluid;
        return flag || !isReplaceable(state, world, pos) ? block : null;
    }

    //BlockFluidBasePlugin helper
    public static boolean isReplaceable(IBlockState state, IBlockAccess world, BlockPos pos) {
        return state.getBlock().isReplaceable(world, pos) || (!state.getMaterial().blocksMovement() && !state.getMaterial().isLiquid());
    }

    //BlockFluidBasePlugin
    public static Material shouldSideBeRendered(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing side) {
        final BlockPos offset = pos.offset(side);
        final FluidState fluidState = FluidState.get(offset);
        final @Nullable Fluid hereFluid = fluidState.isEmpty() ? FluidloggedUtils.getFluidFromState(state) : fluidState.getFluid();
        final boolean canSideFlow = FluidloggedUtils.canFluidFlow(world, offset, state, hereFluid, side.getOpposite());

        if(fluidState.isEmpty()) return state.getMaterial();
        else return canSideFlow ? fluidState.getMaterial() : null;
    }

    //BlockLiquidWrapperPlugin
    public static boolean drainBlockLiquid(World world, BlockPos pos, IBlockState air, int flags) {
        if(!FluidState.get(world, pos).isEmpty())
            return FluidloggedUtils.setFluidState(world, pos, world.getBlockState(pos), FluidState.EMPTY, false, flags);

        //default
        return world.setBlockState(pos, air, flags);
    }

    //BlockLiquidWrapperPlugin
    public static boolean fillBlockLiquid(World world, BlockPos pos, IBlockState state, int flags) {
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromState(state);
        final IBlockState here = world.getBlockState(pos);

        if(FluidloggedUtils.isStateFluidloggable(here, fluid))
            return FluidloggedUtils.setFluidState(world, pos, here, FluidState.of(fluid), true, flags);

        //default
        return world.setBlockState(pos, state, flags);
    }

    //FluidUtilPlugin
    public static IFluidHandler getFluidHandler(World world, BlockPos pos) {
        final FluidState fluidState = FluidState.get(world, pos);

        if(!fluidState.isEmpty()) {
            final Block block = fluidState.getBlock();

            if(block instanceof IFluidBlock)
                return new FluidBlockWrapper((IFluidBlock)block, world, pos);
        }

        return null;
    }

    //FluidUtilPlugin
    public static boolean tryPlaceFluid(World world, BlockPos pos, Fluid fluid, IBlockState destBlockState) {
        return world.isAirBlock(pos) || FluidloggedUtils.isStateFluidloggable(destBlockState, fluid);
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

    //BlockConcretePowderPlugin
    public static boolean tryTouchWater(World world, BlockPos pos, EnumFacing facing) {
        final IBlockState state = world.getBlockState(pos);
        final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos, state);

        return !fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER &&
                FluidloggedUtils.canFluidFlow(world, pos, state, fluidState.getFluid(), facing.getOpposite());
    }

    //BlockDynamicLiquidPlugin
    public static boolean updateTick(BlockDynamicLiquid instance, World world, BlockPos pos, IBlockState state, Random rand) {
        //prevent loading unnecessary chunks
        if(!world.isAreaLoaded(pos, (state.getMaterial() == Material.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 4))
            return false;

        final @Nullable Fluid instanceFluid = FluidloggedUtils.getFluidFromBlock(instance);
        if(instanceFluid == null) throw new IllegalStateException("Fluid block is not a fluid?");

        final IBlockState here = world.getBlockState(pos); //these three variables are helpful down the line & set here as to not have to call getBlockState twice
        final IBlockState down = world.getBlockState(pos.down());

        final int lavaDif = (state.getMaterial() == Material.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
        int stateLevel = state.getValue(BlockLiquid.LEVEL);
        int tickRate = instance.tickRate(world);

        //non source block & non FluidState
        if(stateLevel > 0) {
            int currentMinLevel = -100;
            instance.adjacentSourceBlocks = 0;

            //used in place of instance#checkAdjacentBlock
            for(EnumFacing facing : HORIZONTALS) {
                BlockPos offset = pos.offset(facing);
                IBlockState neighbor = world.getBlockState(offset);
                FluidState fluidNeighbor = FluidloggedUtils.getFluidState(world, offset, neighbor);

                if(fluidNeighbor.getFluid() == instanceFluid && FluidloggedUtils.canFluidFlow(world, offset, neighbor, instanceFluid, facing.getOpposite())) {
                    int neighborLevel = fluidNeighbor.getLevel();

                    if(neighborLevel == 0) instance.adjacentSourceBlocks++;
                    else if(neighborLevel >= 8) neighborLevel = 0;

                    if(currentMinLevel < 0 || neighborLevel < currentMinLevel)
                        currentMinLevel = neighborLevel;
                }
            }

            int futureLevel = currentMinLevel + lavaDif;
            if(futureLevel >= 8 || currentMinLevel < 0) futureLevel = -1;

            //used in place of instance#getDepth
            final IBlockState up = world.getBlockState(pos.up());
            final FluidState fluidStateUp = FluidloggedUtils.getFluidState(world, pos.up(), up);
            final int depth = (fluidStateUp.getFluid() == instanceFluid && FluidloggedUtils.canFluidFlow(world, pos.up(), up, instanceFluid, DOWN)) ? fluidStateUp.getLevel() : -1;
            if(depth >= 0) { futureLevel = (depth >= 8) ? depth : depth + 8; }

            //prepare source block creation
            if(instance.adjacentSourceBlocks >= 2 && ForgeEventFactory.canCreateFluidSource(world, pos, state, state.getMaterial() == Material.WATER)) {
                final FluidState fluidStateDown = FluidloggedUtils.getFluidState(world, pos, down);

                if(down.getMaterial().isSolid())
                    futureLevel = 0;

                else if(fluidStateDown.getFluid() == instanceFluid && fluidStateDown.getLevel() == 0)
                    futureLevel = 0;
            }

            //varies lava flow speed slightly
            if(state.getMaterial() == Material.LAVA && futureLevel < 8 && futureLevel > stateLevel && rand.nextInt(4) != 0)
                tickRate *= 4;

            //reduce lag by placing static block if there's no change
            if(futureLevel == stateLevel) instance.placeStaticBlock(world, pos, state);

            //still a flowing block
            else {
                stateLevel = futureLevel;

                //update state that exists in the world
                if(futureLevel < 0) world.setBlockState(pos, Blocks.AIR.getDefaultState());
                else {
                    state = state.withProperty(BlockLiquid.LEVEL, futureLevel);
                    world.setBlockState(pos, state, Constants.BlockFlags.SEND_TO_CLIENTS);
                    world.scheduleUpdate(pos, instance, tickRate);
                    world.notifyNeighborsOfStateChange(pos, instance, false);
                }
            }
        }

        //set to static block if non-FluidState
        else if(state == here) instance.placeStaticBlock(world, pos, state);

        //flow down
        if(canFlowInto(instance, world, pos.down(), down, DOWN, instanceFluid)) {
            if(state.getMaterial() == Material.LAVA) {
                final FluidState downFluidState = FluidloggedUtils.getFluidState(world, pos.down(), down);
                if(!downFluidState.isEmpty() && downFluidState.getMaterial() == Material.WATER && state.getBlock().isReplaceable(world, pos.down())) {
                    world.setBlockState(pos.down(), ForgeEventFactory.fireFluidPlaceBlockEvent(world, pos.down(), pos, Blocks.STONE.getDefaultState()));
                    instance.triggerMixEffects(world, pos.down());
                    return false;
                }
            }

            if(stateLevel >= 8) tryFlowInto(instance, world, pos.down(), down, DOWN, instanceFluid, stateLevel);
            else tryFlowInto(instance, world, pos.down(), down, DOWN, instanceFluid, stateLevel + 8);
        }

        //flow to side
        else if(stateLevel >= 0 && (stateLevel == 0 || instance.isBlocked(world, pos.down(), down))) {

        }

        //always return false to skip running the old code
        return false;
    }

    //BlockDynamicLiquidPlugin helper
    public static boolean canFlowInto(BlockDynamicLiquid instance, World world, BlockPos pos, IBlockState state, EnumFacing facing, Fluid instanceFluid) {
        if(instance.isBlocked(world, pos, state)) return false;
        final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos, state);
        return fluidState.getFluid() != instanceFluid || !FluidloggedUtils.canFluidFlow(world, pos, state, instanceFluid, facing);
    }

    //BlockLilyPadPlugin
    public static IBlockState canBlockStay(World world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        final @Nullable AxisAlignedBB aabb = state.getCollisionBoundingBox(world, pos);
        return aabb == null || aabb.maxY != 1 ? FluidloggedUtils.getFluidOrReal(world, pos, state) : state;
    }

    //BlockPlugin
    public static boolean canSustainPlant(BlockBush bush, IBlockState state, IBlockAccess world, BlockPos pos) {
        //add special case for lily pads
        if(bush instanceof BlockLilyPad) {
            if(state.getMaterial() == Material.ICE) return true;
            final @Nullable AxisAlignedBB aabb = state.getCollisionBoundingBox(world, pos);
            return (aabb == null || aabb.maxY != 1) && FluidloggedUtils.getFluidOrReal(world, pos, state).getMaterial() == Material.WATER;
        }

        //old code
        return AccessorUtils.canSustainBush(bush, state);
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
    public static boolean checkForFluidOpacity = true;
    @SuppressWarnings("deprecation")
    public static int getLightOpacity(IBlockState state, IBlockAccess world, BlockPos pos) {
        final int here = state.getLightOpacity();
        if(checkForFluidOpacity) {
            //catch loop
            if(FluidloggedUtils.getFluidFromState(state) != null) return here;
            //no fluid here, return old value
            final FluidState fluidState = FluidState.get(world, pos);
            if(fluidState.isEmpty()) return here;
            //compare the fluid & old values and return the greater of the two
            else return Math.max(here, fluidState.getState().getLightOpacity(world, pos));
        }
        //return old opacity value if not checking for FluidState
        else return here;
    }

    //BlockPlugin
    public static boolean checkForFluidLight = true;
    @SuppressWarnings("deprecation")
    public static int getLightValue(IBlockState state, IBlockAccess world, BlockPos pos) {
        final int here = state.getLightValue();
        if(checkForFluidLight) {
            //catch loop
            if(FluidloggedUtils.getFluidFromState(state) != null) return here;
            //no fluid here, return old value
            final FluidState fluidState = FluidState.get(world, pos);
            if(fluidState.isEmpty()) return here;
            //compare the fluid & old values and return the greater of the two
            else return Math.max(here, fluidState.getState().getLightValue(world, pos));
        }
        //return old light value if not checking for FluidState
        else return here;
    }

    //BlockSpongePlugin
    public static boolean canAbsorb = true;
    public static boolean absorb(BlockSponge block, World world, BlockPos origin) {
        if(canAbsorb) {
            canAbsorb = false;

            final List<BlockPos> notifyList = new ArrayList<>();
            final Queue<Pair<BlockPos, Integer>> queue = new LinkedList<>();
            queue.add(Pair.of(origin, 0));

            while(!queue.isEmpty()) {
                Pair<BlockPos, Integer> entry = queue.poll();
                BlockPos pos = entry.getKey();
                int distance = entry.getValue();

                for(EnumFacing facing : EnumFacing.values()) {
                    BlockPos offset = pos.offset(facing);

                    if(FluidloggedUtils.getFluidOrReal(world, offset).getMaterial() == Material.WATER) {
                        if(FluidUtil.tryPickUpFluid(new ItemStack(Items.BUCKET), null, world, offset, facing.getOpposite()).isSuccess()) {
                            if(distance < 6) queue.add(Pair.of(offset, ++distance));
                            notifyList.add(offset);
                        }
                    }
                }

                if(notifyList.size() > 64) break;
            }

            notifyList.forEach(pos -> world.notifyNeighborsOfStateChange(pos, Blocks.AIR, false));
            canAbsorb = true;

            return notifyList.size() > 0;
        }

        else return false;
    }

    //EntityRendererPlugin
    public static boolean addRainParticles(IBlockState here, BlockPos pos) {
        final WorldClient world = Minecraft.getMinecraft().world;
        final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos, here);

        return fluidState.isEmpty() || fluidState.getMaterial() != Material.LAVA
            || !FluidloggedUtils.canFluidFlow(world, pos, here, fluidState.getFluid(), UP);
    }

    //RenderChunkPlugin
    public static boolean renderChunk(Block block, IBlockState state, BlockRenderLayer layerIn, boolean[] array, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, IBlockAccess world, BlockPos pos, BlockPos chunkPos) {
        //only run fluid renderer once
        if(layerIn == BlockRenderLayer.SOLID) {
            final FluidState fluidState = FluidState.get(pos);
            if(!fluidState.isEmpty() && (!(state.getBlock() instanceof IFluidloggable) || ((IFluidloggable)state.getBlock()).shouldFluidRender(world, pos, state, fluidState))) {
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

                    IBakedModel model = Minecraft.getMinecraft().getBlockRendererDispatcher().getModelForState(extendedFluidState);
                    extendedFluidState = getExtendedState(extendedFluidState, world, pos, fluidState.getFluid());

                    //render the fluid
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
        oldState = oldState.getBlock().getExtendedState(oldState, world, pos);
        if(oldState instanceof IExtendedBlockState) {
            IExtendedBlockState state = (IExtendedBlockState)oldState;
            final float corner0 = state.getValue(BlockFluidBase.LEVEL_CORNERS[0]);
            final float corner1 = state.getValue(BlockFluidBase.LEVEL_CORNERS[1]);
            final float corner2 = state.getValue(BlockFluidBase.LEVEL_CORNERS[2]);
            final float corner3 = state.getValue(BlockFluidBase.LEVEL_CORNERS[3]);
            //make sure there's not a fluid block above this prior to performing fixes
            if(corner0 != 1 || corner1 != 1 || corner2 != 1 || corner3 != 1) {
                final float quantaFraction = AccessorUtils.quantaFraction(oldState.getBlock());

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

        return oldState;
    }

    //RenderChunkPlugin helper
    public static boolean fixCorner(IBlockState oldState, IBlockAccess world, BlockPos pos, Fluid fluid, EnumFacing primary, EnumFacing other) {
        if(FluidloggedUtils.canFluidFlow(world, pos, oldState, fluid, primary)) return false;

        final BlockPos offset = pos.offset(other);
        final IBlockState neighbor = world.getBlockState(offset);
        final @Nullable Fluid neighborFluid = FluidloggedUtils.getFluidState(world, offset, neighbor).getFluid();

        if(neighborFluid != fluid) return true;
        else return !FluidloggedUtils.canFluidFlow(world, offset, neighbor, neighborFluid, primary)
                ||  !FluidloggedUtils.canFluidFlow(world, offset, neighbor, neighborFluid, other);
    }

    //WorldClientPlugin
    @SideOnly(Side.CLIENT)
    public static void showBarrierParticles(@Nonnull WorldClient world, int x, int y, int z, int offset, Random random, @Nonnull BlockPos.MutableBlockPos pos) {
        x += world.rand.nextInt(offset) - world.rand.nextInt(offset);
        y += world.rand.nextInt(offset) - world.rand.nextInt(offset);
        z += world.rand.nextInt(offset) - world.rand.nextInt(offset);

        final FluidState fluidState = FluidState.get(pos.setPos(x, y, z));
        if(!fluidState.isEmpty()) fluidState.getBlock().randomDisplayTick(fluidState.getState(), world, pos, random);
    }

    //WorldPlugin
    public static boolean isMaterialInBB(World world, AxisAlignedBB bb, Material materialIn, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
        for(int x = minX; x < maxX; ++x) {
            for(int y = minY; y < maxY; ++y) {
                for(int z = minZ; z < maxZ; ++z) {
                    BlockPos pos = new BlockPos(x, y, z);
                    FluidState fluidState = FluidState.get(world, pos);

                    if(!fluidState.isEmpty()) {
                        @Nullable Boolean result = fluidState.getBlock().isAABBInsideMaterial(world, pos, bb, materialIn);

                        if(Boolean.TRUE.equals(result)) return true;
                        else if(fluidState.getMaterial() == materialIn) return true;
                    }
                }
            }
        }

        return false;
    }

    //WorldPlugin
    public static IBlockState getFluidState(World world, BlockPos pos) {
        final FluidState fluidState = FluidState.get(world, pos);
        return fluidState.isEmpty() ? Blocks.AIR.getDefaultState() : fluidState.getState();
    }

    //WorldPlugin
    public static boolean handleMaterialAcceleration(BlockPos.PooledMutableBlockPos pos, World world, Material material, Entity entity, Vec3d vec3dIn, boolean flagIn, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
        boolean flag = flagIn;

        for(int x = minX; x < maxX; ++x) {
            for(int y = minY; y < maxY; ++y) {
                for(int z = minZ; z < maxZ; ++z) {
                    FluidState fluidState = FluidState.get(world, pos.setPos(x, y, z));
                    if(!fluidState.isEmpty()) {
                        Block block = fluidState.getBlock();

                        @Nullable Boolean result = block.isEntityInsideMaterial(world, pos, fluidState.getState(), entity, maxY, material, false);
                        if(Boolean.TRUE.equals(result)) {
                            Vec3d vec3d = block.modifyAcceleration(world, pos, entity, vec3dIn);
                            vec3dIn.x = vec3d.x;
                            vec3dIn.y = vec3d.y;
                            vec3dIn.z = vec3d.z;
                            flag = true;
                        }

                        else if(!Boolean.FALSE.equals(result) && fluidState.getMaterial() == material) {
                            //check for fluid height
                            double fluidHeight = y + 1 - (1 / 9.0f);
                            if(maxY >= fluidHeight) {
                                Vec3d vec3d = block.modifyAcceleration(world, pos, entity, vec3dIn);
                                vec3dIn.x = vec3d.x;
                                vec3dIn.y = vec3d.y;
                                vec3dIn.z = vec3d.z;
                                flag = true;
                            }
                        }
                    }
                }
            }
        }

        pos.release();
        return flag;
    }

    //WorldPlugin
    public static void neighborChanged(World world, BlockPos pos, Block blockIn, BlockPos fromPos) {
        final FluidState fluidState = FluidState.get(world, pos);
        if(!fluidState.isEmpty()) fluidState.getState().neighborChanged(world, pos, blockIn, fromPos);
    }

    //WorldPlugin
    public static IBlockState setBlockState(Chunk chunk, BlockPos pos, IBlockState newState, IBlockState oldState, World world, int flags) {
        final @Nullable IBlockState chunkState = chunk.setBlockState(pos, newState);
        if(chunkState != null) {
            final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
            //this mod adds a special flag (x | 32, example: Constants.BlockFlags.DEFAULT | 32) that removes any FluidState here
            if((flags & 32) != 0) {
                if(!fluidState.isEmpty())
                    FluidloggedUtils.setFluidState(world, pos, newState, FluidState.EMPTY, false, flags);
            }

            //without the flag preserves FluidState / sets FluidState using oldState if it's a full fluid & if newState is fluidloggable
            else {
                //replace with empty fluid
                if(!fluidState.isEmpty() && !FluidloggedUtils.isStateFluidloggable(newState, fluidState.getFluid()))
                    FluidloggedUtils.setFluidState(world, pos, newState, FluidState.EMPTY, false, flags);

                //save old state as FluidState
                final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromState(oldState);
                if(fluid != null && FluidloggedUtils.isFluidFluidloggable(oldState) && FluidloggedUtils.isStateFluidloggable(newState, fluid))
                    FluidloggedUtils.setFluidState(world, pos, newState, FluidState.of(fluid), false, flags);
            }
        }

        return chunkState;
    }

    //WorldServerPlugin
    public static void updateBlocks(WorldServer world, BlockPos pos) {
        final FluidState fluidState = FluidState.get(world, pos);
        if(!fluidState.isEmpty() && fluidState.getBlock().getTickRandomly()) {
            //special case for lava because it uses BlockStaticLiquid to cause its surroundings to burn
            if(fluidState.getBlock() == Blocks.FLOWING_LAVA)
                Blocks.LAVA.randomTick(world, pos, Blocks.LAVA.getDefaultState(), world.rand);

            //normal cases
            fluidState.getBlock().randomTick(world, pos, fluidState.getState(), world.rand);
        }
        //restore old code
        world.profiler.endSection();
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

    //WorldServerPlugin
    public static boolean tickUpdates(boolean flag, WorldServer world, NextTickListEntry entry) {
        final FluidState fluidState = FluidState.get(world, entry.position);
        if(!fluidState.isEmpty() && Block.isEqualTo(fluidState.getBlock(), entry.getBlock())) {
            try { fluidState.getBlock().updateTick(world, entry.position, fluidState.getState(), world.rand); }
            catch(Throwable throwable) {
                final CrashReport report = CrashReport.makeCrashReport(throwable, "Exception while ticking a fluid");
                CrashReportCategory.addBlockInfo(report.makeCategory("Fluid being ticked"), entry.position, fluidState.getState());

                throw new ReportedException(report);
            }
        }

        return flag;
    }

    //======
    //MODDED
    //======
}
