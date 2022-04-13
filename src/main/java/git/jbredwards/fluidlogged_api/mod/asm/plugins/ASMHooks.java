package git.jbredwards.fluidlogged_api.mod.asm.plugins;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.BFReflector;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.OFReflector;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.util.AccessorUtils;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.chunk.ChunkCompileTaskGenerator;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.crash.CrashReport;
import net.minecraft.crash.CrashReportCategory;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.util.*;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.text.TextComponentString;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.*;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.client.ForgeHooksClient;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.property.PropertyFloat;
import net.minecraftforge.fluids.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;
import static net.minecraft.util.EnumFacing.*;

/**
 * class exists cause SpongeForge
 * @author jbred
 *
 */
@SuppressWarnings("unused")
public final class ASMHooks
{
    //===============================================================
    //INTERNAL (these are given functionality via PluginASMHooks)
    //===============================================================

    @Nullable
    public static Boolean getCanFluidFlow(@Nonnull Block block) { return null; }
    public static void setCanFluidFlow(@Nonnull Block block, @Nullable Boolean canFluidFlowIn) { }

    //=====
    //FORGE
    //=====

    //PluginBlockFluidBase
    @Nonnull
    public static Map<Block, Boolean> defaultDisplacements(@Nonnull Map<Block, Boolean> map) {
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

    //PluginBlockFluidBase
    public static boolean shouldFluidSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side, int densityDir) {
        if(!canFluidFlow(world, pos, world.getBlockState(pos), side)) return true;

        final IBlockState neighbor = world.getBlockState(pos.offset(side));
        return !isCompatibleFluid(getFluidState(world, pos.offset(side), neighbor).getFluid(), getFluidFromState(state))
                || !canFluidFlow(world, pos.offset(side), neighbor, side.getOpposite());
    }

    //PluginBlockFluidBase
    @Nonnull
    public static IBlockState getFluidExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, int densityDir, int quantaPerBlock, float quantaPerBlockFloat, float quantaFraction, float flowDirection) {
        if(!(oldState instanceof IExtendedBlockState)) return oldState;
        final IBlockState here = world.getBlockState(pos);

        //covert to extended state
        IExtendedBlockState state = (IExtendedBlockState)oldState;
        state = state.withProperty(BlockFluidBase.FLOW_DIRECTION, flowDirection);

        //corner height variables
        final IBlockState[][] upBlockState = new IBlockState[3][3];
        final Fluid[][] upFluid = new Fluid[3][3];
        final float[][] height = new float[3][3];
        final float[][] corner = new float[2][2];

        upBlockState[1][1] = world.getBlockState(pos.down(densityDir));
        upFluid[1][1] = getFluidState(world, pos.down(densityDir), upBlockState[1][1]).getFluid();
        height[1][1] = getFluidHeightForRender(fluid, world, pos, upBlockState[1][1], upFluid[1][1], 1, 1, densityDir, quantaPerBlock, quantaPerBlockFloat, quantaFraction);

        //fluid block above this
        if(height[1][1] == 1)
            for(int i = 0; i < 2; i++)
                for(int j = 0; j < 2; j++)
                    corner[i][j] = 1;
        //no fluid block above this
        else {
            //get corner heights from all 8 sides
            for(int i = 0; i < 3; i++) {
                for(int j = 0; j < 3; j++) {
                    if(i != 1 || j != 1) {
                        upBlockState[i][j] = world.getBlockState(pos.add(i - 1, 0, j - 1).down(densityDir));
                        upFluid[i][j] = getFluidState(world, pos.add(i - 1, 0, j - 1).down(densityDir), upBlockState[i][j]).getFluid();
                        height[i][j] = getFluidHeightForRender(fluid, world, pos.add(i - 1, 0, j - 1), upBlockState[i][j], upFluid[i][j], i, j, densityDir, quantaPerBlock, quantaPerBlockFloat, quantaFraction);
                    }
                }
            }
            //find average of all heights for each corner
            for(int i = 0; i < 2; i++)
                for(int j = 0; j < 2; j++)
                    corner[i][j] = getFluidHeightAverage(i, j, quantaFraction, height[i][j], height[i][j + 1], height[i + 1][j], height[i + 1][j + 1]);

            //check for downflow above corners
            boolean n =  isFluid(fluid, upBlockState[0][1], upFluid[0][1], world, pos.north(), EnumFacing.NORTH);
            boolean s =  isFluid(fluid, upBlockState[2][1], upFluid[2][1], world, pos.south(), EnumFacing.SOUTH);
            boolean w =  isFluid(fluid, upBlockState[1][0], upFluid[1][0], world, pos.west(),  EnumFacing.WEST);
            boolean e =  isFluid(fluid, upBlockState[1][2], upFluid[1][2], world, pos.east(),  EnumFacing.EAST);
            boolean nw = isFluid(fluid, upBlockState[0][0], upFluid[0][0], world, pos.north().west(), EnumFacing.NORTH, EnumFacing.WEST);
            boolean ne = isFluid(fluid, upBlockState[0][2], upFluid[0][2], world, pos.north().east(), EnumFacing.NORTH, EnumFacing.EAST);
            boolean sw = isFluid(fluid, upBlockState[2][0], upFluid[2][0], world, pos.south().west(), EnumFacing.SOUTH, EnumFacing.WEST);
            boolean se = isFluid(fluid, upBlockState[2][2], upFluid[2][2], world, pos.south().east(), EnumFacing.SOUTH, EnumFacing.EAST);
            if(nw || n || w) corner[0][0] = 1;
            if(ne || n || e) corner[0][1] = 1;
            if(sw || s || w) corner[1][0] = 1;
            if(se || s || e) corner[1][1] = 1;

            //fix corners of fluidlogged blocks
            if(corner[0][0] < quantaFraction && (fixCorner(fluid, here, world, pos, EnumFacing.NORTH, EnumFacing.WEST) || fixCorner(fluid, here, world, pos, EnumFacing.WEST, EnumFacing.NORTH))) corner[0][0] = quantaFraction;
            if(corner[0][1] < quantaFraction && (fixCorner(fluid, here, world, pos, EnumFacing.SOUTH, EnumFacing.WEST) || fixCorner(fluid, here, world, pos, EnumFacing.WEST, EnumFacing.SOUTH))) corner[0][1] = quantaFraction;
            if(corner[1][0] < quantaFraction && (fixCorner(fluid, here, world, pos, EnumFacing.NORTH, EnumFacing.EAST) || fixCorner(fluid, here, world, pos, EnumFacing.EAST, EnumFacing.NORTH))) corner[1][0] = quantaFraction;
            if(corner[1][1] < quantaFraction && (fixCorner(fluid, here, world, pos, EnumFacing.SOUTH, EnumFacing.EAST) || fixCorner(fluid, here, world, pos, EnumFacing.EAST, EnumFacing.SOUTH))) corner[1][1] = quantaFraction;
        }

        //side overlays
        for(int i = 0; i < 4; i++) {
            EnumFacing side = EnumFacing.byHorizontalIndex(i);
            BlockPos offset = pos.offset(side);
            boolean useOverlay = world.getBlockState(offset).getBlockFaceShape(world, offset, side.getOpposite()) == BlockFaceShape.SOLID;
            state = state.withProperty(BlockFluidBase.SIDE_OVERLAYS[i], useOverlay);
        }

        //fix possible top z fighting
        if(!canFluidFlow(world, pos, here, densityDir < 0 ? EnumFacing.UP : EnumFacing.DOWN)) {
            if(corner[0][0] == 1) corner[0][0] = 0.998f;
            if(corner[0][1] == 1) corner[0][1] = 0.998f;
            if(corner[1][0] == 1) corner[1][0] = 0.998f;
            if(corner[1][1] == 1) corner[1][1] = 0.998f;
        }

        //sets the corner props
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[0], corner[0][0], quantaFraction);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[1], corner[0][1], quantaFraction);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[2], corner[1][1], quantaFraction);
        state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[3], corner[1][0], quantaFraction);
        return state;
    }

    //PluginBlockFluidBase helper
    public static boolean fixCorner(@Nonnull Fluid fluid, @Nonnull IBlockState here, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing primary, @Nonnull EnumFacing other) {
        if(canFluidFlow(world, pos, here, primary)) return false;

        final BlockPos offset = pos.offset(other);
        final IBlockState neighbor = world.getBlockState(offset);

        if(!canFluidFlow(world, offset, neighbor, primary) || !canFluidFlow(world, offset, neighbor, other.getOpposite()))
            return true;

        else return !isCompatibleFluid(getFluidState(world, offset, neighbor).getFluid(), fluid);
    }

    //PluginBlockFluidBase helper
    public static float getFluidHeightForRender(@Nonnull Fluid fluid, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState up, @Nullable Fluid upFluid, int i, int j, int densityDir, int quantaPerBlock, float quantaPerBlockFloat, float quantaFraction) {
        //check block above
        if(isFluid(fluid, up, upFluid, world, pos.down(densityDir), (densityDir < 0 ? EnumFacing.UP : EnumFacing.DOWN))) return 1;

        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        final boolean canSideFlow = canSideFlow(state, world, pos, i, j);
        final boolean fluidMatches = isCompatibleFluid(fluidState.getFluid(), fluid);

        //is a fluid
        if(fluidMatches && canSideFlow) {
            final int level = fluidState.getLevel();

            if(level == 0) return quantaFraction;
            else return ((quantaPerBlock - level) / quantaPerBlockFloat) * quantaFraction;
        }

        //not a fluid
        else return -1f / quantaPerBlock * quantaFraction;
    }

    //PluginBlockFluidBase helper
    public static float getFluidHeightAverage(int i, int j, float quantaFraction, @Nonnull float... flow) {
        float total = 0;
        int count = 0;

        for(int index = 0; index < flow.length; index++) {
            //fix corners visually flowing into illegal sides (vanilla 1.13 bug)
            if(fixN[i] && j == 1 && (index & 1) == 1) continue;
            if(fixS[i] && j == 0 && (index & 1) == 0) continue;
            if(fixE[j] && i == 0 && index <= 1) continue;
            if(fixW[j] && i == 1 && index > 1) continue;

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

    //PluginBlockFluidBase helper
    public static boolean isFluid(@Nonnull Fluid fluid, @Nonnull IBlockState neighbor, @Nullable Fluid other, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing...faces) {
        if(isCompatibleFluid(fluid, other)) {
            for(EnumFacing facing : faces) if(!canFluidFlow(world, pos, neighbor, facing.getOpposite())) return false;
            return true;
        }

        return false;
    }

    //PluginBlockFluidBase helper
    @Nonnull
    public static IExtendedBlockState withPropertyFallback(@Nonnull IExtendedBlockState state, @Nonnull PropertyFloat property, float value, float quantaFraction) {
        return state.withProperty(property, property.isValid(value) ? value : quantaFraction);
    }

    //PluginBlockFluidBase helper (corrects side angles)
    @Nonnull public static boolean[] fixN = new boolean[2];
    @Nonnull public static boolean[] fixS = new boolean[2];
    @Nonnull public static boolean[] fixE = new boolean[2];
    @Nonnull public static boolean[] fixW = new boolean[2];

    //PluginBlockFluidBase helper
    public static boolean canSideFlow(IBlockState state, IBlockAccess world, BlockPos pos, int i, int j) {
        //SE
        if(i == 0 && j == 0) return canSideFlowDir(state, world, pos, SOUTH, EAST);
        //S
        else if(i == 1  && j == 0) {
            fixS = new boolean[2];
            if(canSideFlowDir(state, world, pos, SOUTH)) return true;

            //fix uneven corners
            final boolean flag1 = canSideFlowDir(state, world, pos, SOUTH, EAST);
            final boolean flag2 = canSideFlowDir(state, world, pos, SOUTH, WEST);
            if(flag1 != flag2) {
                if(flag1) fixS[0] = true;
                else      fixS[1] = true;
            }

            return flag1 || flag2;
        }
        //SW
        else if(i == 2  && j == 0) return canSideFlowDir(state, world, pos, SOUTH, WEST);
        //E
        else if(i == 0 && j == 1) {
            fixE = new boolean[2];
            if(canSideFlowDir(state, world, pos, EAST)) return true;

            //fix uneven corners
            final boolean flag1 = canSideFlowDir(state, world, pos, EAST, SOUTH);
            final boolean flag2 = canSideFlowDir(state, world, pos, EAST, NORTH);
            if(flag1 != flag2) {
                if(flag1) fixE[0] = true;
                else      fixE[1] = true;
            }

            return flag1 || flag2;
        }
        //W
        else if(i == 2  && j == 1) {
            fixW = new boolean[2];
            if(canSideFlowDir(state, world, pos, WEST)) return true;

            //fix uneven corners
            final boolean flag1 = canSideFlowDir(state, world, pos, WEST, SOUTH);
            final boolean flag2 = canSideFlowDir(state, world, pos, WEST, NORTH);
            if(flag1 != flag2) {
                if(flag1) fixW[0] = true;
                else      fixW[1] = true;
            }

            return flag1 || flag2;
        }
        //NE
        else if(i == 0 && j == 2) return canSideFlowDir(state, world, pos, NORTH, EAST);
        //N
        else if(i == 1  && j == 2) {
            fixN = new boolean[2];
            if(canSideFlowDir(state, world, pos, NORTH)) return true;

            //fix uneven corners
            final boolean flag1 = canSideFlowDir(state, world, pos, NORTH, EAST);
            final boolean flag2 = canSideFlowDir(state, world, pos, NORTH, WEST);
            if(flag1 != flag2) {
                if(flag1) fixN[0] = true;
                else      fixN[1] = true;
            }

            return flag1 || flag2;
        }
        //NW
        else if(i == 2  && j == 2) return canSideFlowDir(state, world, pos, NORTH, WEST);
        //default
        else return true;
    }

    //PluginBlockFluidBase helper
    public static boolean canSideFlowDir(IBlockState state, IBlockAccess world, BlockPos pos, EnumFacing... sides) {
        for(EnumFacing side : sides) {
            if(canFluidFlow(world, pos, state, side) && canFluidFlow(world, pos.offset(side), world.getBlockState(pos.offset(side)), side.getOpposite()))
                return true;
        }

        return false;
    }

    //PluginFluidUtil
    public static boolean tryPlaceFluid(World world, BlockPos pos, Fluid fluid, IBlockState destBlockState) {
        return world.isAirBlock(pos) || isStateFluidloggable(destBlockState, world, pos, fluid);
    }

    //PluginModelFluid
    public static float fixTextureFightingZ(float old, int index) {
        final EnumFacing facing = EnumFacing.byHorizontalIndex((5 - index) % 4); // [W, S, E, N]
        if(facing.getAxis() == Axis.X) return old;
        else return old == 1 ? 0.998f : 0.002f;
    }

    //PluginModelFluid
    public static float fixTextureFightingX(float old, int index) {
        final EnumFacing facing = EnumFacing.byHorizontalIndex((5 - index) % 4); // [W, S, E, N]
        if(facing.getAxis() == Axis.Z) return old;
        else return old == 1 ? 0.998f : 0.002f;
    }

    //=======
    //VANILLA
    //=======

    //PluginBlock
    public static boolean canSustainPlant(BlockBush bush, IBlockState state, IBlockAccess world, BlockPos pos) {
        //add special case for lily pads
        if(bush instanceof BlockLilyPad) {
            return state.getMaterial() == Material.ICE
                    || state.getBoundingBox(world, pos).maxY < 1
                    && getFluidOrReal(world, pos, state).getMaterial() == Material.WATER;
        }

        //old code
        return AccessorUtils.canSustainBush(bush, state);
    }

    //PluginBlock
    public float getExplosionResistance(@Nonnull Block block, @Nullable Entity exploder, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Explosion explosion) {
        if(getFluidFromBlock(block) != null) return block.getExplosionResistance(exploder);
        //return the greater of the two possible resistance values here
        final FluidState fluidState = FluidState.get(world, pos);
        return Math.max(fluidState.isEmpty() ? 0 : fluidState.getBlock().getExplosionResistance(world, pos, exploder, explosion), block.getExplosionResistance(exploder));
    }

    //PluginBlock
    public static int getLightOpacity(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(getFluidFromState(state) != null) return state.getLightOpacity();
        //return the greater of the two possible light values here
        final FluidState fluidState = FluidState.get(world, pos);
        return Math.max(fluidState.isEmpty() ? 0 : fluidState.getState().getLightOpacity(world, pos), state.getLightOpacity());
    }

    //PluginBlock
    public static int getLightValue(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        if(getFluidFromState(state) != null) return state.getLightValue();
        //return the greater of the two possible light values here
        final FluidState fluidState = FluidState.get(world, pos);
        return Math.max(fluidState.isEmpty() ? 0 : fluidState.getState().getLightValue(world, pos), state.getLightValue());
    }

    //PluginBlockBarrier
    public static void fixBarrierParticles(IBlockState state, World world, BlockPos pos) {
        final EntityPlayer player = Minecraft.getMinecraft().player;
        if(player.isCreative() && player.getHeldItemMainhand().getItem() == state.getBlock().getItemDropped(state, world.rand, 0))
            world.spawnParticle(EnumParticleTypes.BARRIER, pos.getX() + 0.5, pos.getY() + 0.5, pos.getZ() + 0.5, 0, 0, 0);
    }

    //PluginBlockConcretePowder
    public static boolean tryTouchWater(World world, BlockPos pos, EnumFacing facing) {
        final IBlockState state = world.getBlockState(pos);
        return getFluidOrReal(world, pos, state).getMaterial() == Material.WATER
                && canFluidFlow(world, pos, state, facing.getOpposite());
    }

    //PluginBlockDoor
    public static void notifyDoorFluids(@Nonnull World world, @Nonnull BlockPos rangeMin, @Nonnull BlockPos rangeMax) {
        notifyFluids(world, rangeMin.up(), FluidState.get(world, rangeMin.up()), false, EnumFacing.DOWN);
        world.markBlockRangeForRenderUpdate(rangeMin, rangeMax);
    }

    //PluginBlockDoor
    public static boolean canDoorFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull EnumFacing side) {
        if(side.getAxis().isVertical()) return true;

        here = here.getActualState(world, pos);
        final EnumFacing facing = here.getValue(BlockDoor.FACING);

        return (here.getValue(BlockDoor.OPEN) ? (here.getValue(BlockDoor.HINGE) == BlockDoor.EnumHingePosition.RIGHT
                ? facing.rotateY() : facing.rotateYCCW()) : facing.getOpposite()) != side;
    }

    //PluginBlockLilyPad
    public static IBlockState canBlockStay(World world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        final AxisAlignedBB aabb = state.getBoundingBox(world, pos);
        return aabb.maxY < 1 ? getFluidOrReal(world, pos, state) : state;
    }

    //PluginBlockTrapDoor
    public static boolean canTrapDoorFluidFlow(IBlockState here, EnumFacing side) {
        final boolean isOpen = here.getValue(BlockTrapDoor.OPEN);

        if(side.getAxis().isHorizontal()) return !isOpen || here.getValue(BlockTrapDoor.FACING).getOpposite() != side;
        else if(side == EnumFacing.UP) return isOpen || here.getValue(BlockTrapDoor.HALF) == BlockTrapDoor.DoorHalf.BOTTOM;
        else return isOpen || here.getValue(BlockTrapDoor.HALF) == BlockTrapDoor.DoorHalf.TOP;
    }

    //PluginBlockWall
    public static boolean shouldHavePost(IBlockAccess world, BlockPos pos) {
        return world.isAirBlock(pos) || getFluidFromState(world.getBlockState(pos)) != null;
    }

    //PluginEntityRenderer
    public static boolean addRainParticles(IBlockState here, BlockPos pos) {
        final WorldClient world = Minecraft.getMinecraft().world;
        final FluidState fluidState = getFluidState(world, pos, here);

        return fluidState.isEmpty() || fluidState.getMaterial() != Material.LAVA
            || !canFluidFlow(world, pos, here, UP);
    }

    //PluginRenderChunk
    public static boolean renderChunk(Block block, IBlockState state, BlockRenderLayer layerIn, boolean[] array, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, IBlockAccess world, BlockPos pos, BlockPos chunkPos) {
        //only run fluid renderer once
        if(layerIn.ordinal() == 0 && getFluidFromState(state) == null) {
            final FluidState fluidState = FluidState.get(pos);
            if(!fluidState.isEmpty() && fluidState.getState().getRenderType() == EnumBlockRenderType.MODEL
                    && (!(state.getBlock() instanceof IFluidloggable) || ((IFluidloggable)state.getBlock()).shouldFluidRender(world, pos, state, fluidState))) {

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

                    //give mods a chance to change something about the rendered fluid
                    IBlockState extendedFluidState = world.getWorldType() != WorldType.DEBUG_ALL_BLOCK_STATES ?
                            fluidState.getState().getActualState(world, pos) : fluidState.getState();

                    IBakedModel model = Minecraft.getMinecraft().getBlockRendererDispatcher().getModelForState(extendedFluidState);
                    extendedFluidState = fluidState.getBlock().getExtendedState(extendedFluidState, world, pos);

                    //render the fluid
                    array[layer.ordinal()] |= Minecraft.getMinecraft().getBlockRendererDispatcher().getBlockModelRenderer()
                            .renderModel(world, model, extendedFluidState, pos, buffer, true);
                }

                //reset current render layer
                ForgeHooksClient.setRenderLayer(null);
            }
        }

        //always return old code
        return canRenderBlockInLayer(block, state, layerIn);
    }

    //PluginRenderChunk
    public static boolean renderChunkOF(Block block, IBlockState state, BlockRenderLayer layerIn, boolean[] array, RenderChunk renderChunk, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, IBlockAccess world, BlockPos pos, BlockPos chunkPos) {
        //only run fluid renderer once
        if(layerIn.ordinal() == 0 && getFluidFromState(state) == null) {
            final FluidState fluidState = FluidState.get(pos);
            if(!fluidState.isEmpty() && fluidState.getState().getRenderType() == EnumBlockRenderType.MODEL
                    && (!(state.getBlock() instanceof IFluidloggable) || ((IFluidloggable)state.getBlock()).shouldFluidRender(world, pos, state, fluidState))) {

                //renders the fluid in each layer
                for(BlockRenderLayer layer : BlockRenderLayer.values()) {
                    if(!fluidState.getBlock().canRenderInLayer(fluidState.getState(), layer))
                        continue;

                    ForgeHooksClient.setRenderLayer(layer);
                    BufferBuilder buffer = generator.getRegionRenderCacheBuilder().getWorldRendererByLayer(layer);

                    try {
                        OFReflector.setBlockLayer.invoke(buffer, layer);

                        Object renderEnv = OFReflector.getRenderEnv.invoke(buffer, fluidState.getState(), pos);
                        OFReflector.setRegionRenderCacheBuilder.invoke(renderEnv, generator.getRegionRenderCacheBuilder());

                        if(!compiledChunk.isLayerStarted(layer)) {
                            compiledChunk.setLayerStarted(layer);
                            buffer.begin(7, DefaultVertexFormats.BLOCK);
                            buffer.setTranslation(-chunkPos.getX(), -chunkPos.getY(), -chunkPos.getZ());
                        }

                        //give mods a chance to change something about the rendered fluid
                        IBlockState extendedFluidState = world.getWorldType() != WorldType.DEBUG_ALL_BLOCK_STATES ?
                                fluidState.getState().getActualState(world, pos) : fluidState.getState();

                        IBakedModel model = Minecraft.getMinecraft().getBlockRendererDispatcher().getModelForState(extendedFluidState);
                        extendedFluidState = fluidState.getBlock().getExtendedState(extendedFluidState, world, pos);

                        //render the fluid
                        array[layer.ordinal()] |= Minecraft.getMinecraft().getBlockRendererDispatcher().getBlockModelRenderer()
                                .renderModel(world, model, extendedFluidState, pos, buffer, true);

                        //post shader stuff
                        if((boolean)OFReflector.isOverlaysRendered.invoke(renderEnv)) {
                            OFReflector.postRenderOverlays.invoke(renderChunk, generator.getRegionRenderCacheBuilder(), compiledChunk, array);
                            OFReflector.setOverlaysRendered.invoke(renderEnv, false);
                        }
                    }
                    //shouldn't catch, but if it does, alert the player
                    catch(Exception e) { Minecraft.getMinecraft().player.sendMessage(new TextComponentString(TextFormatting.RED + e.toString())); }
                }

                //reset current render layer
                ForgeHooksClient.setRenderLayer(null);
            }
        }

        return canRenderBlockInLayer(block ,state, layerIn);
    }

    //PluginRenderChunk
    public static boolean renderChunkOF(Object blockIn, Object ignored, Object[] args, boolean[] array, RenderChunk renderChunk, ChunkCompileTaskGenerator generator, CompiledChunk compiledChunk, IBlockAccess world, BlockPos pos, BlockPos chunkPos) {
        return renderChunkOF((Block)blockIn, (IBlockState)args[0], (BlockRenderLayer)args[1], array, renderChunk, generator, compiledChunk, world, pos, chunkPos);
    }

    //PluginRenderChunk helper
    public static boolean canRenderBlockInLayer(Block block, IBlockState state, BlockRenderLayer layer) {
        try {
            return BFReflector.canRenderBlockInLayer != null
                    ? (boolean)BFReflector.canRenderBlockInLayer.invoke(null, block, state, layer)
                    : block.canRenderInLayer(state, layer);
        }
        catch (IllegalAccessException | InvocationTargetException e) {
            return block.canRenderInLayer(state, layer);
        }
    }

    //PluginWorld
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

    //PluginWorld
    public static IBlockState getFluidOrAir(World world, BlockPos pos) {
        final FluidState fluidState = FluidState.get(world, pos);
        return fluidState.isEmpty() ? Blocks.AIR.getDefaultState() : fluidState.getState();
    }

    //PluginWorld
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

    //PluginWorld
    public static void neighborChanged(World world, BlockPos pos, Block blockIn, BlockPos fromPos) {
        final FluidState fluidState = FluidState.get(world, pos);
        if(!fluidState.isEmpty()) fluidState.getState().neighborChanged(world, pos, blockIn, fromPos);
    }

    //PluginWorld
    public static IBlockState setBlockState(Chunk chunk, BlockPos pos, IBlockState newState, IBlockState oldState, World world, int flags) {
        final @Nullable IBlockState chunkState = chunk.setBlockState(pos, newState);
        if(chunkState != null) {
            final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
            //this mod adds a special flag (x | 32, example: Constants.BlockFlags.DEFAULT | 32) that removes any FluidState here
            if((flags & 32) != 0) {
                if(!fluidState.isEmpty())
                    setFluidState(world, pos, newState, FluidState.EMPTY, false, flags);
            }

            //without the flag preserves FluidState / sets FluidState using oldState if it's a full fluid & if newState is fluidloggable
            else {
                if(!fluidState.isEmpty()) {
                    //update neighboring fluids, there's been a state change
                    if(isStateFluidloggable(newState, world, pos, fluidState.getFluid()))
                        notifyFluids(world, pos, fluidState, true);

                    //remove fluid here, new state isn't fluidloggable
                    else setFluidState(world, pos, newState, FluidState.EMPTY, false, flags);
                }
                //save oldState as FluidState
                else {
                    final @Nullable Fluid fluid = getFluidFromState(oldState);
                    if(fluid != null && isFluidloggableFluid(oldState, true) && isStateFluidloggable(newState, world, pos, fluid))
                        setFluidState(world, pos, newState, FluidState.of(fluid), false, flags);
                }
            }
        }

        return chunkState;
    }

    //PluginWorldServer
    public static void updateBlocks(WorldServer world, BlockPos pos) {
        final FluidState fluidState = FluidState.get(world, pos);
        if(!fluidState.isEmpty() && fluidState.getBlock().getTickRandomly())
            fluidState.getBlock().randomTick(world, pos, fluidState.getState(), world.rand);

        //restore old code
        world.profiler.endSection();
    }

    //PluginWorldServer
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

    //PluginWorldServer
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
}
