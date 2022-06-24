package git.jbredwards.fluidlogged_api.mod.asm.plugins;

import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.BFReflector;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.OFReflector;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import net.minecraft.block.*;
import net.minecraft.block.material.Material;
import net.minecraft.block.material.MaterialLogic;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.chunk.ChunkCompileTaskGenerator;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.crash.CrashReport;
import net.minecraft.crash.CrashReportCategory;
import net.minecraft.entity.Entity;
import net.minecraft.entity.passive.EntityWaterMob;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.*;
import net.minecraft.util.math.*;
import net.minecraft.util.text.TextComponentString;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.*;
import net.minecraft.world.chunk.Chunk;
import net.minecraft.world.gen.structure.template.PlacementSettings;
import net.minecraft.world.gen.structure.template.Template;
import net.minecraftforge.client.ForgeHooksClient;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.common.property.PropertyFloat;
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
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.function.Supplier;

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
        if(!canFluidFlow(world, pos, world.getBlockState(pos), side) && !FluidState.get(world, pos).isEmpty()) return true;
        final IBlockState neighbor = world.getBlockState(pos.offset(side));
        //this check exists for mods like coral reef that don't have proper block sides
        if(isCompatibleFluid(getFluidFromState(state), getFluidFromState(neighbor))) return false;
        else if(side != (densityDir > 0 ? DOWN : UP) && neighbor.doesSideBlockRendering(world, pos.offset(side), side.getOpposite())) return false;
        return !isCompatibleFluid(getFluidState(world, pos.offset(side), neighbor).getFluid(), getFluidFromState(state))
                || !canFluidFlow(world, pos.offset(side), neighbor, side.getOpposite());
    }

    //PluginBlockFluidBase
    @Nonnull
    public static IBlockState getFluidExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, int densityDir, int quantaPerBlock, float quantaPerBlockFloat, float quantaFraction, float flowDirection) {
        if(!(oldState instanceof IExtendedBlockState)) return oldState;
        final EnumFacing densityFace = densityDir < 0 ? UP : DOWN;

        //covert to extended state
        IExtendedBlockState state = (IExtendedBlockState)oldState;
        state = state.withProperty(BlockFluidBase.FLOW_DIRECTION, flowDirection);

        //corner height variables
        final IBlockState[][][] states = new IBlockState[3][2][3];
        final FluidState[][][] fluids = new FluidState[3][2][3];
        final float[][] height = new float[3][3];
        final float[][] corner = new float[2][2];

        //initialize here states
        states[1][0][1] = world.getBlockState(pos);
        states[1][1][1] = world.getBlockState(pos.down(densityDir));
        fluids[1][0][1] = getFluidState(world, pos, states[1][0][1]);
        fluids[1][1][1] = getFluidState(world, pos.down(densityDir), states[1][1][1]);
        height[1][1] = getFluidHeightForRender(world, pos, states, fluids, 1, 1, densityFace, quantaPerBlock, quantaPerBlockFloat, quantaFraction);

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
                        if(states[i][0][j] == null) states[i][0][j] = world.getBlockState(pos.add(i - 1, 0, j - 1));
                        if(states[i][1][j] == null) states[i][1][j] = world.getBlockState(pos.add(i - 1, -densityDir, j - 1));
                        if(fluids[i][0][j] == null) fluids[i][0][j] = getFluidState(world, pos.add(i - 1, 0, j - 1), states[i][0][j]);
                        if(fluids[i][1][j] == null) fluids[i][1][j] = getFluidState(world, pos.add(i - 1, -densityDir, j - 1), states[i][1][j]);
                        height[i][j] = getFluidHeightForRender(world, pos, states, fluids, i, j, densityFace, quantaPerBlock, quantaPerBlockFloat, quantaFraction);
                    }
                }
            }
            //find average of gathered heights for each corner
            for(int i = 0; i < 2; i++)
                for(int j = 0; j < 2; j++)
                    corner[i][j] = getFluidHeightAverage(quantaFraction, height[i][j], height[i][j + 1], height[i + 1][j], height[i + 1][j + 1]);
        }

        //side overlays, skipped if there's no overlay texture
        if(fluid.getOverlay() != null) {
            for(int i = 0; i < 4; i++) {
                EnumFacing side = byHorizontalIndex(i);
                BlockPos offset = pos.offset(side);
                //use cache if available
                state = state.withProperty(BlockFluidBase.SIDE_OVERLAYS[i], !canFluidFlow(world, offset,
                        getOrSet(states, () -> world.getBlockState(offset), side.getXOffset() + 1, side.getZOffset() + 1),
                        side.getOpposite()));
            }
        }

        //fix possible top z fighting
        if(!canFluidFlow(world, pos, states[1][0][1], densityFace)) {
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
    @Nonnull
    private static final EnumFacing[][] FACES = {
            {NORTH, WEST}, {NORTH}, {NORTH, EAST},
                {WEST},    {     },     {EAST},
            {SOUTH, WEST}, {SOUTH}, {SOUTH, EAST},
    };

    //PluginBlockFluidBase helper
    public static float getFluidHeightForRender(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState[][][] states, @Nonnull FluidState[][][] fluids, int i, int j, EnumFacing densityFace, int quantaPerBlock, float quantaPerBlockFloat, float quantaFraction) {
        final EnumFacing[] faces = FACES[j * 3 + i];

        //check for vertical connections
        if(connectToVertical(states, fluids, world, pos, densityFace, i, j, faces))
            return 1;

        //check for horizontal connections
        if(connectToHorizontal(states, fluids, world, pos, i, j, faces)) {
            //air block
            if(states[i][0][j].getBlock().isAir(states[i][0][j], world, pos.add(i - 1, 0, j - 1)))
                return 0;

            //fluid block
            else if(isCompatibleFluid(fluids[1][0][1].getFluid(), fluids[i][0][j].getFluid())) {
                int lowest = fluids[i][0][j].getLevel();
                if(lowest == 0) return quantaFraction;

                //diagonals check if nearby have lower levels to connect
                if(faces.length > 1) {
                    for(EnumFacing facing : faces) {
                        EnumFacing other = faces[facing.getXOffset() != 0 ? 0 : 1];
                        BlockPos offset = pos.offset(facing);
                        IBlockState neighbor = getOrSet(states, () -> world.getBlockState(offset), facing);

                        //check indirect adjacent
                        if((!canFluidFlow(world, pos, states[1][0][1], other)
                                || !canFluidFlow(world, pos.offset(other), getOrSet(states, () -> world.getBlockState(pos.offset(other)), other), other.getOpposite()))
                                && canFluidFlow(world, pos, states[1][0][1], facing)
                                && canFluidFlow(world, offset, neighbor, facing.getOpposite())
                                && canFluidFlow(world, offset, neighbor, other)
                                && canFluidFlow(world, pos.add(i - 1, 0, j - 1), states[i][0][j], other.getOpposite())
                                && isCompatibleFluid(fluids[1][0][1].getFluid(), getOrSet(fluids, () -> getFluidState(world, offset, neighbor), facing).getFluid())
                                && canFluidFlow(world, pos.add(i - 1, 0, j - 1), states[i][0][j], facing.getOpposite())
                                && canFluidFlow(world, pos.offset(other), getOrSet(states, () -> world.getBlockState(pos.offset(other)), other), facing)
                                && isCompatibleFluid(fluids[1][0][1].getFluid(), getOrSet(fluids, () -> getFluidState(world, pos.offset(other), get(states, other)), other).getFluid())
                                && lowest > get(fluids, other).getLevel()) lowest = get(fluids, other).getLevel();
                    }
                }

                //connect to neighbor fluid at the correct height
                return ((quantaPerBlock - lowest) / quantaPerBlockFloat) * quantaFraction;
            }
        }

        //default
        return -1;
    }

    //PluginBlockFluidBase helper
    public static float getFluidHeightAverage(float quantaFraction, @Nonnull float... heights) {
        float total = 0;
        int count = 0;

        for(float height : heights) {
            if(height == 1) //vertical fluid
                return 1;

            if(height >= quantaFraction) {
                total += height * 10;
                count += 10;
            }

            if(height >= 0) {
                total += height;
                count++;
            }
        }

        return total / count;
    }

    //PluginBlockFluidBase helper
    public static boolean connectToVertical(@Nonnull IBlockState[][][] states, @Nonnull FluidState[][][] fluids, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing densityFace, int i, int j, @Nonnull EnumFacing[] sides) {
        return isCompatibleFluid(fluids[1][0][1].getFluid(), fluids[i][1][j].getFluid())
                && isCompatibleFluid(fluids[1][0][1].getFluid(), fluids[i][0][j].getFluid())
                && canFluidFlow(world, pos.add(i - 1, 0, j - 1), states[i][0][j], densityFace)
                && canFluidFlow(world, pos.add(i - 1, -densityFace.getYOffset(), j - 1), states[i][1][j], densityFace.getOpposite())
                && connectToHorizontal(states, fluids, world, pos, i, j, sides);
    }

    //PluginBlockFluidBase helper
    public static boolean connectToHorizontal(@Nonnull IBlockState[][][] states, @Nonnull FluidState[][][] fluids, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int i, int j, @Nonnull EnumFacing[] sides) {
        final boolean diagonal = sides.length > 1;
        for(EnumFacing facing : sides) {
            //check diagonal
            if(diagonal) {
                EnumFacing other = sides[facing.getXOffset() != 0 ? 0 : 1];
                BlockPos offset = pos.offset(facing);
                IBlockState neighbor = getOrSet(states, () -> world.getBlockState(offset), facing);

                if(canFluidFlow(world, pos, states[1][0][1], facing)
                        && canFluidFlow(world, offset, neighbor, facing.getOpposite())
                        && canFluidFlow(world, offset, neighbor, other)
                        && canFluidFlow(world, pos.add(i - 1, 0, j - 1), states[i][0][j], other.getOpposite())
                        && isCompatibleFluid(fluids[1][0][1].getFluid(), getOrSet(fluids, () -> getFluidState(world, offset, neighbor), facing).getFluid()))

                    return true;
            }
            //check adjacent
            else if(!canFluidFlow(world, pos, states[1][0][1], facing) || !canFluidFlow(world, pos.offset(facing), states[i][0][j], facing.getOpposite()))
                return false;
        }

        return !diagonal;
    }

    //PluginBlockFluidBase helper
    @Nonnull
    public static <T> T get(@Nonnull T[][][] values, @Nonnull EnumFacing facing) { return values[facing.getXOffset() + 1][0][facing.getZOffset() + 1]; }

    //PluginBlockFluidBase helper
    @Nonnull
    public static <T> T getOrSet(@Nonnull T[][][] values, @Nonnull Supplier<T> fallback, @Nonnull EnumFacing facing) {
        return getOrSet(values, fallback, facing.getXOffset() + 1, facing.getZOffset() + 1);
    }

    //PluginBlockFluidBase helper
    @Nonnull
    public static <T> T getOrSet(@Nonnull T[][][] values, @Nonnull Supplier<T> fallback, int i, int j) {
        if(values[i][0][j] != null) return values[i][0][j];
        else return values[i][0][j] = fallback.get();
    }

    //PluginBlockFluidBase helper
    @Nonnull
    public static IExtendedBlockState withPropertyFallback(@Nonnull IExtendedBlockState state, @Nonnull PropertyFloat property, float value, float quantaFraction) {
        return state.withProperty(property, property.isValid(value) ? value : quantaFraction);
    }

    //PluginBlockFluidBase
    @Nonnull
    public static Vec3d getFluidFlowVector(@Nonnull BlockFluidBase block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int densityDir, int quantaPerBlock) {
        final IBlockState here = world.getBlockState(pos);
        Vec3d vec = Vec3d.ZERO;

        final int decay = ASMNatives.getFlowDecay(block, world, pos);
        for(EnumFacing facing : HORIZONTALS) {
            if(canFluidFlow(world, pos, here, facing)) {
                BlockPos offset = pos.offset(facing);
                if(canFluidFlow(world, offset, world.getBlockState(offset), facing.getOpposite())) {
                    int otherDecay = ASMNatives.getFlowDecay(block, world, offset);

                    if(otherDecay >= quantaPerBlock) {
                        otherDecay = ASMNatives.getFlowDecay(block, world, offset.up(densityDir));

                        if(otherDecay < quantaPerBlock) {
                            int power = otherDecay - (decay - quantaPerBlock);
                            vec = vec.add(facing.getXOffset() * power, 0, facing.getZOffset() * power);
                        }
                    }
                    else {
                        int power = otherDecay - decay;
                        vec = vec.add(facing.getXOffset() * power, 0, facing.getZOffset() * power);
                    }
                }
            }
        }

        return vec.normalize();
    }

    //PluginBlockFluidBase
    public static boolean hasVerticalFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, int densityDir) {
        final EnumFacing facing = (densityDir < 0) ? UP : DOWN;
        if(!canFluidFlow(world, pos, world.getBlockState(pos), facing)) return false;

        final BlockPos offset = pos.down(densityDir);
        final IBlockState state = world.getBlockState(offset);

        return canFluidFlow(world, offset, state, facing.getOpposite())
                && isCompatibleFluid(getFluidState(world, offset, state).getFluid(), fluid);
    }

    //PluginBlockFluidBase
    @Nonnull
    public static IBlockState getStateAtViewpoint(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Vec3d viewpoint) {
        final IBlockState here = world.getBlockState(pos);
        return here == state ? Blocks.AIR.getDefaultState()
                : here.getBlock().getStateAtViewpoint(here, world, pos, viewpoint);
    }

    //PluginBlockFluidBase
    @Nonnull
    public static Vec3d getFluidFogColor(@Nonnull BlockFluidBase block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Entity entity, @Nonnull Vec3d originalColor, float partialTicks) {
        //remove built-in in place for better check
        if(isWithinFluid(block, (IExtendedBlockState)block.getExtendedState(state, world, pos), world, pos, ActiveRenderInfo.projectViewFromEntity(entity, partialTicks))) {
            int color = block.getFluid().getColor();
            float red = (color >> 16 & 0xFF) / 255.0f;
            float green = (color >> 8 & 0xFF) / 255.0f;
            float blue = (color & 0xFF) / 255.0f;
            return new Vec3d(red, green, blue);
        }

        //not inside fluid
        return originalColor;
    }

    //PluginBlockFluidBase
    @Nullable
    public static Boolean isEntityInsideFluid(@Nonnull Block block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Entity entity, double yToTest, @Nonnull Material materialIn, boolean testingHead) {
        if(materialIn != state.getMaterial()) return null;
        else if(!testingHead) return isWithinFluid(block, world, pos, entity.getEntityBoundingBox());
        return isWithinFluid(block, (IExtendedBlockState)block.getExtendedState(state, world, pos), world, pos, new Vec3d(entity.posX, yToTest, entity.posZ));
    }

    //PluginBlockFluidBase
    @Nullable
    public static Boolean isAABBInsideMaterial(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB boundingBox, @Nonnull Material materialIn) {
        return materialIn != block.getDefaultState().getMaterial() ? null : block.isAABBInsideLiquid(world, pos, boundingBox);
    }

    //PluginBlockFluidBase
    public static boolean isWithinFluid(@Nonnull Block block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb) {
        bb = bb.intersect(new AxisAlignedBB(pos.getX(), pos.getY(), pos.getZ(), pos.getX() + 1, pos.getY() + 1, pos.getZ() + 1));
        final IExtendedBlockState state = (IExtendedBlockState)block.getExtendedState(getFluidOrReal(world, pos), world, pos);
        return isWithinFluid(block, state, world, pos, new Vec3d(bb.minX, bb.minY, bb.minZ))
                || isWithinFluid(block, state, world, pos, new Vec3d(bb.minX, bb.minY, bb.maxZ))
                || isWithinFluid(block, state, world, pos, new Vec3d(bb.maxX, bb.minY, bb.minZ))
                || isWithinFluid(block, state, world, pos, new Vec3d(bb.maxX, bb.minY, bb.maxZ));
    }

    //PluginBlockFluidBase helper
    public static boolean isWithinFluid(@Nonnull Block block, @Nonnull IExtendedBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Vec3d entityVec) {
        final float[][] corners = new float[2][2];
        corners[0][0] = state.getValue(BlockFluidBase.LEVEL_CORNERS[0]);
        corners[0][1] = state.getValue(BlockFluidBase.LEVEL_CORNERS[1]);
        corners[1][1] = state.getValue(BlockFluidBase.LEVEL_CORNERS[2]);
        corners[1][0] = state.getValue(BlockFluidBase.LEVEL_CORNERS[3]);

        double x = entityVec.x - pos.getX();
        double z = entityVec.z - pos.getZ();
        if(x < 0) x++;
        if(z < 0) z++;

        final double dif1 = Math.sqrt(2) - distance(0, x, 0, z);
        final double dif2 = Math.sqrt(2) - distance(0, x, 1, z);
        final double dif3 = Math.sqrt(2) - distance(1, x, 1, z);
        final double dif4 = Math.sqrt(2) - distance(1, x, 0, z);
        final double fluidHeight = (corners[0][0] * dif1 + corners[0][1] * dif2 + corners[1][1] * dif3 + corners[1][0] * dif4)
                / (dif1 + dif2 + dif3 + dif4);

        //if fluid is upside down, do upside down collision
        return block instanceof BlockFluidBase && ((BlockFluidBase)block).getDensity() < 0
                ? entityVec.y > pos.getY() - fluidHeight + 1 : entityVec.y < pos.getY() + fluidHeight;
    }

    //PluginBlockFluidBase helper
    public static double distance(double x1, double x2, double z1, double z2) {
        final double distX = Math.max(x1, x2) - Math.min(x1, x2);
        final double distZ = Math.max(z1, z2) - Math.min(z1, z2);
        return Math.sqrt(distX * distX + distZ * distZ);
    }

    //PluginBlockFluidClassic
    public static int getQuantaValue(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int quantaPerBlock) {
        final IBlockState here = world.getBlockState(pos);
        if(here.getBlock().isAir(here, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, here);
        return isCompatibleFluid(fluidState.getFluid(), block.getFluid())
                ? quantaPerBlock - fluidState.getLevel() : -1;
    }

    //PluginBlockFluidClassic
    private static final List<EnumFacing> SIDES = Collections.unmodifiableList(Arrays.asList(WEST, EAST, NORTH, SOUTH));
    public static void fluidUpdateTick(@Nonnull BlockFluidClassic block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, int quantaPerBlock, int densityDir, boolean canCreateSources) {
        if(!world.isAreaLoaded(pos, quantaPerBlock / 2)) return; // Forge: avoid loading unloaded chunks

        final IBlockState here = world.getBlockState(pos); //fluidlogged fluids will have a different state here than the state input
        final EnumFacing facingDir = (densityDir > 0) ? UP : DOWN;
        int quantaRemaining = quantaPerBlock - state.getValue(BlockLiquid.LEVEL);

        // check adjacent block levels if non-source
        if(quantaRemaining < quantaPerBlock) {
            int adjacentSourceBlocks = 0;
            final int expQuanta;

            if(ForgeEventFactory.canCreateFluidSource(world, pos, state, canCreateSources)) {
                for(EnumFacing facing : HORIZONTALS) {
                    BlockPos offset = pos.offset(facing);

                    if(isSourceBlock(block, world, offset, world.getBlockState(offset), facing.getOpposite()))
                        adjacentSourceBlocks++;
                }
            }

            // new source block
            final IBlockState vertical = world.getBlockState(pos.up(densityDir));
            if(adjacentSourceBlocks >= 2 && (vertical.getMaterial().isSolid() || isSourceBlock(block, world, pos.up(densityDir), vertical, facingDir.getOpposite())))
                expQuanta = quantaPerBlock;

            // vertical flow into block
            else if(ASMNatives.hasVerticalFlow(block, world, pos)) expQuanta = quantaPerBlock - 1;

            else {
                int maxQuanta = -100;
                for(EnumFacing side : HORIZONTALS) {
                    BlockPos offset = pos.offset(side);

                    if(canFluidFlow(world, pos, here, side) && canFluidFlow(world, offset, world.getBlockState(offset), side.getOpposite()))
                        maxQuanta = ASMNatives.getLargerQuanta(block, world, offset, maxQuanta);
                }

                expQuanta = maxQuanta - 1;
            }

            // decay calculation
            if(expQuanta != quantaRemaining) {
                quantaRemaining = expQuanta;

                if(expQuanta <= 0) world.setBlockToAir(pos);
                else {
                    world.setBlockState(pos, state.withProperty(BlockLiquid.LEVEL, quantaPerBlock - expQuanta), Constants.BlockFlags.SEND_TO_CLIENTS);
                    world.scheduleUpdate(pos, block, block.tickRate(world));
                    world.notifyNeighborsRespectDebug(pos, block, false);
                }
            }
        }

        //try flowing to nearby fluidloggable blocks
        else tryFlowIntoFluidloggable(block, world, pos, facingDir, state, here, quantaPerBlock, canCreateSources, HORIZONTALS);

        // Fluidlog vertical if possible
        if(ConfigHandler.verticalFluidloggedFluidSpread)
            tryFlowIntoFluidloggable(block, world, pos, facingDir, state, here, quantaPerBlock, canCreateSources, facingDir);

        // Flow vertically if possible
        if(canFluidFlow(world, pos, here, facingDir) && block.canDisplace(world, pos.up(densityDir))) {
            ASMNatives.flowIntoBlock(block, world, pos.up(densityDir), 1);
            return;
        }

        // Flow outward if possible
        int flowMeta = quantaPerBlock - quantaRemaining + 1;
        if(flowMeta >= quantaPerBlock) return;

        if(isSourceBlock(block, world, pos, here, null) || !block.isFlowingVertically(world, pos)) {
            if(ASMNatives.hasVerticalFlow(block, world, pos)) flowMeta = 1;

            final boolean[] flowTo = ASMNatives.getOptimalFlowDirections(block, world, pos);
            for(int i = 0; i < 4; i++)
                if(flowTo[i] && canFluidFlow(world, pos, here, SIDES.get(i)))
                    ASMNatives.flowIntoBlock(block, world, pos.offset(SIDES.get(i)), flowMeta);
        }
    }

    //PluginBlockFluidClassic helper
    public static void tryFlowIntoFluidloggable(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumFacing facingDir, @Nonnull IBlockState state, @Nonnull IBlockState here, int quantaPerBlock, boolean canCreateSources, @Nonnull EnumFacing... flowInto) {
        if(quantaPerBlock > 0 && ConfigHandler.fluidloggedFluidSpread > 0 && canCreateSources && (ConfigHandler.fluidloggedFluidSpread == 2 || state != here) && (state != here || isFluidloggableFluid(state, world, pos))) {
            for(EnumFacing facing : flowInto) {
                if(canFluidFlow(world, pos, here, facing)) {
                    BlockPos offset = pos.offset(facing);
                    IBlockState neighbor = world.getBlockState(offset);

                    //check if the fluid could occupy the space
                    if(canFluidFlow(world, offset, neighbor, facing.getOpposite()) && isStateFluidloggable(neighbor, world, offset, block.getFluid()) && FluidState.get(world, offset).isEmpty()) {
                        //check for another source block that can flow into this
                        for(EnumFacing adjacentFacing : values()) {
                            if(adjacentFacing != facingDir && adjacentFacing != facing.getOpposite() && (adjacentFacing.getYOffset() == 0 || ConfigHandler.verticalFluidloggedFluidSpread) && canFluidFlow(world, offset, neighbor, adjacentFacing)) {
                                BlockPos adjacentOffset = offset.offset(adjacentFacing);
                                IBlockState adjacent = world.getBlockState(adjacentOffset);

                                if(canFluidFlow(world, adjacentOffset, adjacent, adjacentFacing.getOpposite())) {
                                    //only allow certain FluidStates to count
                                    FluidState adjacentFluid = ConfigHandler.fluidloggedFluidSpread == 1
                                            ? FluidState.get(world, adjacentOffset)
                                            : getFluidState(world, adjacentOffset, adjacent);

                                    //set the FluidState in the world
                                    if(isCompatibleFluid(adjacentFluid.getFluid(), block.getFluid()) && isFluidloggableFluid(adjacentFluid.getState(), world, adjacentOffset)) {
                                        setFluidState(world, offset, neighbor, FluidState.of(block.getFluid()), false);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    //PluginBlockFluidClassic
    public static boolean isFluidFlowingVertically(@Nonnull BlockFluidClassic block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int densityDir) {
        final EnumFacing facingDir = (densityDir < 0) ? UP : DOWN;

        final IBlockState here = world.getBlockState(pos);
        if(!canFluidFlow(world, pos, here, facingDir.getOpposite())) return false;

        final IBlockState neighbor = world.getBlockState(pos.up(densityDir));
        return isCompatibleFluid(getFluidState(world, pos.up(densityDir), neighbor).getFluid(), block.getFluid())
                || (isCompatibleFluid(getFluidState(world, pos, here).getFluid(), block.getFluid())
                && ASMNatives.canFlowInto(block, world, pos.up(densityDir)));
    }

    //PluginBlockFluidClassic
    public static boolean canFlowInto(@Nonnull BlockFluidClassic block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        return isCompatibleFluid(getFluidFromState(state), block.getFluid()) && state.getValue(BlockLiquid.LEVEL) > 0 || block.canDisplace(world, pos);
    }

    //PluginBlockFluidClassic
    public static boolean isSourceBlock(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nullable EnumFacing facing) {
        if(facing != null && !canFluidFlow(world, pos, here, facing)) return false;

        final FluidState fluidState = getFluidState(world, pos, here);
        return isCompatibleFluid(fluidState.getFluid(), block.getFluid()) && fluidState.getLevel() == 0;
    }

    //PluginBlockFluidClassic
    public static boolean getOptimalFlowDirections(@Nonnull BlockFluidClassic block, @Nonnull IBlockAccess world, @Nonnull BlockPos offset, @Nonnull BlockPos pos, @Nonnull IBlockState here, int side) {
        return !canFluidFlow(world, pos, here, SIDES.get(side)) || block.isSourceBlock(world, offset);
    }

    //PluginBlockFluidClassic
    public static int place(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace, @Nonnull IBlockState defaultState) {
        if(fluidStack.amount < Fluid.BUCKET_VOLUME) return 0;
        if(doPlace) {
            final IBlockState here = world.getBlockState(pos);

            if(isStateFluidloggable(here, world, pos, block.getFluid())) {
                if(!setFluidState(world, pos, here, FluidState.of(block.getFluid()), true)) return 0; }
            else world.setBlockState(pos, defaultState, Constants.BlockFlags.DEFAULT_AND_RERENDER);
        }

        return Fluid.BUCKET_VOLUME;
    }

    //PluginBlockFluidClassic
    @Nullable
    public static FluidStack drain(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, boolean doDrain, @Nullable FluidStack stack) {
        final IBlockState here = world.getBlockState(pos);
        final FluidState fluidState = getFluidState(world, pos, here);

        if(fluidState.isEmpty()) return null;
        if(doDrain) {
            if(fluidState.getState() == here) world.setBlockState(pos, Blocks.AIR.getDefaultState());
            else if(!setFluidState(world, pos, here, FluidState.EMPTY, false)) return null;
        }

        return fluidState.getLevel() != 0 ? null : stack == null ? new FluidStack(block.getFluid(), Fluid.BUCKET_VOLUME) : stack.copy();
    }

    //PluginBlockFluidClassic
    public static boolean isFluidFluidloggable(@Nonnull IFluidloggableFluid block, @Nonnull IBlockState fluid, @Nonnull World world, @Nonnull BlockPos pos, int densityDir, boolean canCreateSources) {
        if(!block.isFluidloggableFluid()) return false;
        else if(fluid.getValue(BlockLiquid.LEVEL) == 0) return true;
        else if(!canCreateSources) return false;

        final IBlockState vertical = world.getBlockState(pos.down(densityDir));
        return isCompatibleFluid(block.getFluid(), getFluidState(world, pos.down(densityDir), vertical).getFluid())
                && canFluidFlow(world, pos.down(densityDir), vertical, densityDir < 1 ? DOWN : UP);
    }

    //PluginBlockFluidClassic
    public static boolean isFluidFluidloggable(@Nonnull Block block) {
        final @Nullable Fluid fluid = getFluidFromBlock(block);
        return fluid != null && block == fluid.getBlock() && !block.hasTileEntity();
    }

    //PluginFluidUtil
    @Nullable
    public static IFluidHandler getFluidHandler(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        final FluidState fluidState = getFluidState(world, pos, state);
        return fluidState.isEmpty() ? null : new FluidBlockWrapper(fluidState.getBlock(), world, pos);
    }

    //PluginFluidUtil
    public static boolean tryPlaceFluid(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, @Nonnull IBlockState destBlockState) {
        return world.isAirBlock(pos) || isStateFluidloggable(destBlockState, world, pos, fluid);
    }

    //PluginModelFluid
    public static float fixTextureFightingZ(float old, int index) {
        final EnumFacing facing = byHorizontalIndex((5 - index) % 4); // [W, S, E, N]
        if(facing.getAxis() == Axis.X) return old;
        else return old == 1 ? 0.998f : 0.002f;
    }

    //PluginModelFluid
    public static float fixTextureFightingX(float old, int index) {
        final EnumFacing facing = byHorizontalIndex((5 - index) % 4); // [W, S, E, N]
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
        return ASMNatives.canSustainBush(bush, state);
    }

    //PluginBlock
    public static float getExplosionResistance(@Nonnull Block block, @Nullable Entity exploder, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Explosion explosion) {
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
    @SideOnly(Side.CLIENT)
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
        notifyFluids(world, rangeMin.up(), FluidState.get(world, rangeMin.up()), false, DOWN);
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

    //PluginBlockDynamicLiquid
    public static void randomLiquidTick(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random random) {
        if(state.getMaterial() == Material.LAVA) Blocks.LAVA.randomTick(worldIn, pos, state, random);
    }

    //PluginBlockDynamicLiquid
    public static void neighborChanged(@Nonnull BlockLiquid block, @Nonnull IBlockState state, @Nonnull World worldIn, @Nonnull BlockPos pos) {
        if(!block.checkForMixing(worldIn, pos, state) && state != worldIn.getBlockState(pos)) worldIn.scheduleUpdate(pos, block, block.tickRate(worldIn));
    }

    //PluginBlockDynamicLiquid
    public static void updateLiquidTick(@Nonnull BlockDynamicLiquid block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
        final int lavaDif = (state.getMaterial() == Material.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
        if(!world.isAreaLoaded(pos, 3 - lavaDif)) return; // Forge: avoid loading unloaded chunks

        final @Nullable Fluid fluid = getFluidFromBlock(block);
        if(fluid == null) throw new IllegalStateException("A critical error has occurred, please issue a bug report!");

        //fluidlogged fluids will have a different state here than the state input
        final IBlockState here = world.getBlockState(pos);
        int quantaRemaining = 8 - state.getValue(BlockLiquid.LEVEL);

        // check adjacent block levels if non-source
        if(quantaRemaining < 8) {
            int adjacentSourceBlocks = 0;
            final int expQuanta;

            if(ForgeEventFactory.canCreateFluidSource(world, pos, state, state.getMaterial() == Material.WATER)) {
                for(EnumFacing facing : HORIZONTALS) {
                    BlockPos offset = pos.offset(facing);

                    if(isSourceBlock((IFluidBlock)block, world, offset, world.getBlockState(offset), facing.getOpposite()))
                        adjacentSourceBlocks++;
                }
            }

            // new source block
            final IBlockState vertical = world.getBlockState(pos.down());
            if(adjacentSourceBlocks >= 2 && (vertical.getMaterial().isSolid() || isSourceBlock((IFluidBlock)block, world, pos.down(), vertical, UP)))
                expQuanta = 8;

            // vertical flow into block
            else if(hasVerticalFlow(world, pos, fluid, -1)) expQuanta = 8 - lavaDif;

            else {
                int maxQuanta = -100;
                for(EnumFacing side : HORIZONTALS) {
                    BlockPos offset = pos.offset(side);

                    if(canFluidFlow(world, pos, here, side) && canFluidFlow(world, offset, world.getBlockState(offset), side.getOpposite()))
                        maxQuanta = getLargerQuanta((IFluidBlock)block, world, offset, maxQuanta);
                }

                expQuanta = maxQuanta - lavaDif;
            }

            //place static block
            if(expQuanta == quantaRemaining) block.placeStaticBlock(world, pos, state);

                // decay calculation
            else {
                quantaRemaining = expQuanta;

                if(expQuanta <= 0 ) world.setBlockToAir(pos);
                else {
                    world.setBlockState(pos, state.withProperty(BlockLiquid.LEVEL, 8 - expQuanta), Constants.BlockFlags.SEND_TO_CLIENTS);
                    world.scheduleUpdate(pos, block, block.tickRate(world));
                    world.notifyNeighborsRespectDebug(pos, block, false);
                }
            }
        }

        //place static block
        else {
            tryFlowIntoFluidloggable((IFluidBlock)block, world, pos, DOWN, state, here, 8, state.getMaterial() == Material.WATER, HORIZONTALS);
            if(state == here) block.placeStaticBlock(world, pos, state);
        }

        // Fluidlog vertical if possible
        if(ConfigHandler.verticalFluidloggedFluidSpread)
            tryFlowIntoFluidloggable((IFluidBlock)block, world, pos, DOWN, state, here, 8, state.getMaterial() == Material.WATER, DOWN);

        // Flow vertically if possible
        if(canFluidFlow(world, pos, here, DOWN) && canDisplace(world, pos.down(), fluid)) {
            if(state.getMaterial() == Material.LAVA) {
                final IBlockState down = world.getBlockState(pos.down());
                if(down.getBlock().isReplaceable(world, pos.down()) && canFluidFlow(world, pos.down(), down, UP)) {
                    final FluidState fluidState = getFluidState(world, pos.down(), down);
                    if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER) {
                        world.setBlockState(pos.down(), ForgeEventFactory.fireFluidPlaceBlockEvent(world, pos.down(), pos, Blocks.STONE.getDefaultState()));
                        block.triggerMixEffects(world, pos.down());
                        return;
                    }
                }
            }

            flowIntoBlock(block, world, pos.down(), fluid, lavaDif);
            return;
        }

        // Flow outward if possible
        int flowMeta = 8 - quantaRemaining + lavaDif;
        if(flowMeta >= 8 || flowMeta <= 0) return;

        if(isSourceBlock((IFluidBlock)block, world, pos, here, null) || !hasVerticalFlow(world, pos.down(), fluid, -1)) {
            final boolean[] flowTo = getOptimalFlowDirections((IFluidBlock)block, world, pos, here);
            for(int i = 0; i < 4; i++)
                if(flowTo[i] && canFluidFlow(world, pos, here, SIDES.get(i)))
                    flowIntoBlock(block, world, pos.offset(SIDES.get(i)), fluid, flowMeta);
        }
    }

    //PluginBlockDynamicLiquid helper
    public static boolean canDisplace(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Fluid fluid) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return true;
        //checks if this & a fluid here are the same
        else if(isCompatibleFluid(fluid, getFluidState(world, pos, state).getFluid())) return false;
        //predefined displacements
        else if(displacements().containsKey(state.getBlock())) return displacements.get(state.getBlock());

        final Material material = state.getMaterial();
        if(material.blocksMovement() || material == Material.PORTAL || material == Material.STRUCTURE_VOID) return false;

        final int density = BlockFluidBase.getDensity(world, pos);
        return density == Integer.MAX_VALUE || fluid.getDensity() > density;
    }

    //PluginBlockDynamicLiquid helper
    private static Map<Block, Boolean> displacements = null;
    public static Map<Block, Boolean> displacements() {
        if(displacements == null) displacements = defaultDisplacements(new HashMap<>());
        return displacements;
    }

    //PluginBlockDynamicLiquid helper
    public static int getLargerQuanta(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare) {
        int quantaRemaining = hasVerticalFlow(world, pos, block.getFluid(), -1) ? 8 : getQuantaValue(block, world, pos);
        if(quantaRemaining <= 0) return compare;

        return Math.max(quantaRemaining, compare);
    }

    //PluginBlockDynamicLiquid helper
    public static boolean[] getOptimalFlowDirections(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
        final int lavaDif = (block.getFluid() == FluidRegistry.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
        final int[] flowCost = { 1000, 1000, 1000, 1000 };

        for(int side = 0; side < 4; side++) {
            if(!canFluidFlow(world, pos, here, SIDES.get(side))) continue;

            BlockPos offset = pos.offset(SIDES.get(side));
            IBlockState neighbor = world.getBlockState(offset);
            if(!canFlowInto(block, world, offset, neighbor) || isSourceBlock(block, world, offset, neighbor, null)) continue;

            if(canFlowInto(block, world, offset.down(), world.getBlockState(offset.down()))) flowCost[side] = 0;
            else flowCost[side] = calculateFlowCost(block, world, offset, lavaDif, side);
        }

        final int min = Ints.min(flowCost);
        final boolean[] isOptimalFlowDirection = new boolean[4];
        for(int side = 0; side < 4; side++) isOptimalFlowDirection[side] = (flowCost[side] == min);

        return isOptimalFlowDirection;
    }

    //PluginBlockDynamicLiquid helper
    public static int calculateFlowCost(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, int recurseDepth, int side) {
        final int lavaDif = (block.getFluid() == FluidRegistry.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
        int cost = 1000;

        for(int adjSide = 0; adjSide < 4; adjSide++) {
            if(SIDES.get(adjSide) == SIDES.get(side).getOpposite()) continue;
            BlockPos pos2 = pos.offset(SIDES.get(adjSide));
            IBlockState neighbor = world.getBlockState(pos2);

            if(!canFlowInto(block, world, pos2, neighbor) || isSourceBlock(block, world, pos2, neighbor, null)) continue;
            else if(canFlowInto(block, world, pos2.down(), world.getBlockState(pos2.down()))) return recurseDepth;
            else if(recurseDepth > 2) continue;

            cost = Math.min(cost, calculateFlowCost(block, world, pos2, recurseDepth + lavaDif, adjSide));
        }

        return cost;
    }

    //PluginBlockDynamicLiquid helper
    public static boolean canFlowInto(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        return isCompatibleFluid(getFluidFromState(state), block.getFluid()) && state.getValue(BlockLiquid.LEVEL) > 0 || canDisplace(world, pos, block.getFluid());
    }

    //PluginBlockDynamicLiquid helper
    public static void flowIntoBlock(@Nonnull BlockDynamicLiquid block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, int meta) {
        if(canDisplace(world, pos, fluid)) {
            final IBlockState state = world.getBlockState(pos);

            if(!state.getBlock().isAir(state, world, pos)) {
                // Forge: Vanilla has a 'bug' where snowballs don't drop like every other block. So special case because ewww...
                if(state.getBlock() != Blocks.SNOW_LAYER) state.getBlock().dropBlockAsItem(world, pos, state, 0);
                if(fluid == FluidRegistry.LAVA) block.triggerMixEffects(world, pos);
            }

            world.setBlockState(pos, block.getDefaultState().withProperty(BlockLiquid.LEVEL, meta));
        }
    }

    //PluginBlockLilyPad
    public static IBlockState canBlockStay(World world, BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        final AxisAlignedBB aabb = state.getBoundingBox(world, pos);
        return aabb.maxY < 1 ? getFluidOrReal(world, pos, state) : state;
    }

    //PluginBlockLiquid
    @Nonnull
    public static BlockStateContainer createLiquidBlockState(@Nonnull Block block) {
        return new BlockStateContainer.Builder(block)
                .add(BlockFluidBase.FLUID_RENDER_PROPS.toArray(new IUnlistedProperty<?>[0]))
                .add(BlockLiquid.LEVEL).build();
    }

    //PluginBlockLiquid
    public static boolean checkForMixing(@Nonnull BlockLiquid block, @Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState stateIn) {
        if(stateIn.getMaterial() == Material.LAVA) {
            //get state here, since this method can also be executed from a FluidState
            final IBlockState here = worldIn.getBlockState(pos);
            if(here.getBlock().isReplaceable(worldIn, pos)) {
                for(EnumFacing facing : values()) {
                    if(facing == DOWN  || !canFluidFlow(worldIn, pos, here, facing))
                        continue;

                    BlockPos offset = pos.offset(facing);
                    IBlockState state = worldIn.getBlockState(offset);

                    if(!canFluidFlow(worldIn, offset, state, facing.getOpposite())) continue;
                    FluidState fluidState = getFluidState(worldIn, offset, state);

                    if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER) {
                        final int level = stateIn.getValue(BlockLiquid.LEVEL);

                        worldIn.setBlockState(pos, ForgeEventFactory.fireFluidPlaceBlockEvent(worldIn, pos, pos, level == 0 ? Blocks.OBSIDIAN.getDefaultState() : Blocks.COBBLESTONE.getDefaultState()));
                        block.triggerMixEffects(worldIn, pos);
                        return true;
                    }
                }
            }
        }

        return false;
    }

    //PluginBlockLiquid
    @Nonnull
    public static Vec3d getFlow(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
        final int decay = 8 - getEffectiveQuanta(block, world, pos);
        Vec3d vec = Vec3d.ZERO;

        for(EnumFacing facing : HORIZONTALS) {
            if(canFluidFlow(world, pos, here, facing)) {
                BlockPos offset = pos.offset(facing);

                if(canFluidFlow(world, offset, world.getBlockState(offset), facing.getOpposite())) {
                    int otherDecay = 8 - getEffectiveQuanta(block, world, offset);

                    if(otherDecay >= 8) {
                        otherDecay = 8 - getEffectiveQuanta(block, world, offset.down());

                        if(otherDecay < 8) {
                            int power = otherDecay - (decay - 8);
                            vec = vec.add(facing.getXOffset() * power, 0, facing.getZOffset() * power);
                        }
                    }
                    else {
                        int power = otherDecay - decay;
                        vec = vec.add(facing.getXOffset() * power, 0, facing.getZOffset() * power);
                    }
                }
            }
        }

        return vec.normalize();
    }

    //PluginBlockLiquid helper
    public static int getEffectiveQuanta(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        int quantaValue = getQuantaValue(block, world, pos);
        return quantaValue > 0 && quantaValue < 8 && hasVerticalFlow(world, pos, block.getFluid(), -1) ? 8 : quantaValue;
    }

    //PluginBlockLiquid helper
    public static int getQuantaValue(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        if(!isCompatibleFluid(fluidState.getFluid(), block.getFluid())) return -1;

        final int level = fluidState.getLevel();
        return level >= 8 ? 8 : 8 - level;
    }

    //PluginBlockLiquid
    public static float getBlockLiquidHeight(@Nonnull IBlockState state, @Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) {
        final IBlockState up = worldIn.getBlockState(pos.up());
        final boolean flag = isCompatibleFluid(getFluidState(worldIn, pos.up(), up).getFluid(), getFluidFromState(state))
                && canFluidFlow(worldIn, pos.up(), up, DOWN)
                && canFluidFlow(worldIn, pos, worldIn.getBlockState(pos), UP);

        return flag ? 1 : 1 - (BlockLiquid.getLiquidHeightPercent(state.getValue(BlockLiquid.LEVEL)) - 1f/9);
    }

    //PluginBlockLiquid helper, exists to fix issue#59
    public static double getSlopeAngle(@Nonnull BlockLiquid block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final Vec3d vec = block.getFlow(world, pos, world.getBlockState(pos));
        return vec.x == 0 && vec.z == 0 ? -1000 : MathHelper.atan2(vec.z, vec.x) - Math.PI / 2;
    }

    //PluginBlockLiquid
    public static boolean getLiquidFogColor(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Vec3d viewport) {
        return !isWithinFluid(state.getBlock(), (IExtendedBlockState)state.getBlock().getExtendedState(state, world, pos), world, pos, viewport);
    }

    //PluginBlockLiquid
    @Nonnull
    public static Fluid getLiquid(@Nonnull Material material) { return material == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA; }

    //PluginBlockLiquid
    public static boolean canDrain(@Nonnull World world, @Nonnull BlockPos pos) { return getFluidState(world, pos).getLevel() == 0; }

    //PluginBlockLiquid
    public static boolean isLiquidFluidloggable(@Nonnull IFluidloggableFluid block, @Nonnull IBlockState fluid, @Nonnull World world, @Nonnull BlockPos pos) {
        if(!block.isFluidloggableFluid()) return false;
        else if(fluid.getValue(BlockLiquid.LEVEL) == 0) return true;
        else if(fluid.getMaterial() != Material.WATER) return false;

        final IBlockState vertical = world.getBlockState(pos.up());
        return isCompatibleFluid(block.getFluid(), getFluidState(world, pos.up(), vertical).getFluid())
                && canFluidFlow(world, pos.up(), vertical, DOWN);
    }

    //PluginBlockLiquid
    public static boolean isLiquidFluidloggable(@Nonnull Block block) {
        //most modded BlockLiquid instances involve blocks that shouldn't be fluidloggable fluids (like coral)
        return block == Blocks.WATER || block == Blocks.LAVA || block == Blocks.FLOWING_WATER || block == Blocks.FLOWING_LAVA;
    }

    //PluginBlockLiquid
    @Nonnull
    public static IBlockState getStateAtViewpoint(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Vec3d viewpoint) {
        if(isWithinFluid(state.getBlock(), (IExtendedBlockState)state.getBlock().getExtendedState(state, world, pos), world, pos, viewpoint)) return state;
        //return the other block here if the player isn't within the fluid
        final IBlockState here = world.getBlockState(pos);
        return here == state ? Blocks.AIR.getDefaultState()
                : here.getBlock().getStateAtViewpoint(here, world, pos, viewpoint);
    }

    //PluginBlockPistonBase
    public static boolean isPistonFluidloggable(@Nonnull IBlockState state) { return state.getValue(BlockPistonBase.EXTENDED); }

    //PluginBlockSlab
    public static boolean isSlabFluidloggable(@Nonnull BlockSlab slab) { return !slab.isDouble(); }

    //PluginBlockSponge
    private static boolean available = true;
    public static boolean absorb(@Nonnull World world, @Nonnull BlockPos origin) {
        //temporary fix for strange update bug
        if(available) {
            available = false;
            final Queue<Pair<BlockPos, Integer>> queue = new LinkedList<>();
            queue.add(Pair.of(origin, 0));
            int absorbed = 0;

            while(!queue.isEmpty()) {
                final Pair<BlockPos, Integer> entry = queue.poll();
                final BlockPos pos = entry.getKey();
                final int distance = entry.getValue();

                for(EnumFacing facing : values()) {
                    final BlockPos offset = pos.offset(facing);
                    final FluidState fluidState = getFluidState(world, offset);

                    if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER) {
                        //don't drain bad fluid blocks (looking at you BOP kelp)
                        if(fluidState.isValid()) {
                            fluidState.getBlock().drain(world, offset, true);
                            if(distance < 6) queue.add(Pair.of(offset, distance + 1));
                            absorbed++;
                        }
                        //drain bad fluid blocks
                        else if(world.setBlockToAir(pos)) {
                            world.playEvent(Constants.WorldEvents.BREAK_BLOCK_EFFECTS, offset, Block.getStateId(fluidState.getState()));
                            fluidState.getBlock().dropBlockAsItem(world, offset, fluidState.getState(), 0);
                            if(distance < 6) queue.add(Pair.of(offset, distance + 1));
                            absorbed++;
                        }
                    }
                }
            }

            available = true;
            return absorbed > 0;
        }

        return false;
    }

    //PluginBlockTrapDoor
    public static boolean canTrapDoorFluidFlow(IBlockState here, EnumFacing side) {
        final boolean isOpen = here.getValue(BlockTrapDoor.OPEN);

        if(side.getAxis().isHorizontal()) return !isOpen || here.getValue(BlockTrapDoor.FACING).getOpposite() != side;
        else if(side == UP) return isOpen || here.getValue(BlockTrapDoor.HALF) == BlockTrapDoor.DoorHalf.BOTTOM;
        else return isOpen || here.getValue(BlockTrapDoor.HALF) == BlockTrapDoor.DoorHalf.TOP;
    }

    //PluginBlockTorch
    @Nonnull private static final Material TORCH = new MaterialLogic(Material.CIRCUITS.getMaterialMapColor()) {
        @Nonnull
        @Override
        public Material setNoPushMobility() { return super.setNoPushMobility(); }
    }.setNoPushMobility();

    //PluginBlockTorch
    @Nonnull
    public static Material getTorchMaterial(@Nonnull Material material, @Nonnull Block block) {
        return !ConfigHandler.fluidsBreakTorches || block instanceof BlockRedstoneTorch ? material : TORCH;
    }

    //PluginBlockWall
    public static boolean shouldHavePost(IBlockAccess world, BlockPos pos) {
        return world.isAirBlock(pos) || getFluidFromState(world.getBlockState(pos)) != null;
    }

    //PluginChunkCache
    @Nullable
    public static Chunk getChunkFromChunkCache(@Nonnull BlockPos pos, @Nonnull Chunk[][] chunkArray, int chunkX, int chunkZ) {
        final int x = (pos.getX() >> 4) - chunkX;
        final int z = (pos.getZ() >> 4) - chunkZ;
        return x >= 0 && x < chunkArray.length && z >= 0 && z < chunkArray[x].length ? chunkArray[x][z] : null;
    }

    //PluginEntity
    public static float doWaterSplashEffect(@Nonnull Entity entity) {
        final @Nullable RayTraceResult result = entity.world.rayTraceBlocks(
                new Vec3d(entity.posX - entity.motionX, entity.posY - entity.motionY, entity.posZ - entity.motionZ),
                new Vec3d(entity.posX, entity.posY, entity.posZ),
                true, true, false);

        //use the exact point where the entity collided with water
        if(result != null) {
            final BlockPos pos = result.getBlockPos();
            final FluidState fluidState = getFluidState(entity.world, pos);
            if(!fluidState.isEmpty() && fluidState.isValid()) {
                final float filled = fluidState.getBlock().getFilledPercentage(entity.world, pos);
                return pos.getY() + (filled < 0 ? filled + 1.1f : filled - 0.1f);
            }
        }

        //estimate (should never pass)
        return (float)(entity.posY + entity.motionY * -0.7 - 0.1);
    }

    //PluginEntity
    public static double fixSquidWaterCollision(double factor, @Nonnull Entity entity) { return entity instanceof EntityWaterMob ? factor : 0; }

    //PluginEntity
    @Nullable
    public static Boolean isEntityInsideFluidState(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull Entity entity, double yToTest, @Nonnull Material materialIn, boolean testingHead) {
        @Nullable Boolean result = block.isEntityInsideMaterial(world, pos, here, entity, yToTest, materialIn, testingHead);
        if(result != null) return result;
        //check for FluidState if block here is not a fluid
        else if(getFluidFromBlock(block) == null) {
            final FluidState fluidState = FluidState.get(world, pos);
            if(!fluidState.isEmpty()) {
                result = fluidState.getBlock().isEntityInsideMaterial(world, pos, fluidState.getState(), entity, yToTest, materialIn, testingHead);
                if(result != null) return result;
                else if(fluidState.getMaterial() == materialIn)
                    return ForgeHooks.isInsideOfMaterial(materialIn, entity, pos);
            }
        }

        return null;
    }

    //PluginEntity
    public static void onEntityCollidedWithFluidState(@Nonnull Block block, @Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull Entity entityIn) {
        //check if the entity is inside the block before doing collisions
        if(!Boolean.FALSE.equals(block.isAABBInsideLiquid(worldIn, pos, entityIn.getEntityBoundingBox())))
            block.onEntityCollision(worldIn, pos, here, entityIn);

        //don't check for FluidState if block here is a fluid
        if(getFluidFromBlock(block) != null) return;
        final FluidState fluidState = FluidState.get(worldIn, pos);
        if(!fluidState.isEmpty() && !Boolean.FALSE.equals(fluidState.getBlock().isAABBInsideLiquid(worldIn, pos, entityIn.getEntityBoundingBox())))
            fluidState.getBlock().onEntityCollision(worldIn, pos, fluidState.getState(), entityIn);
    }

    //PluginEntityRenderer
    public static boolean addRainParticles(IBlockState here, BlockPos pos) {
        final WorldClient world = Minecraft.getMinecraft().world;
        final FluidState fluidState = getFluidState(world, pos, here);

        return fluidState.isEmpty() || fluidState.getMaterial() != Material.LAVA
            || !canFluidFlow(world, pos, here, UP);
    }

    //PluginMaterialLogic
    public static boolean isMaterialCircuit(@Nonnull Material material) { return material == Material.CIRCUITS; }

    //PluginParticle
    public static int fixParticleBrightness(@Nonnull World world, @Nonnull BlockPos pos) {
        final int sky = fixParticleBrightness(world, pos, EnumSkyBlock.SKY);
        final int block = fixParticleBrightness(world, pos, EnumSkyBlock.BLOCK);
        return sky << 20 | Math.max(block, 0) << 4;
    }

    //PluginParticle helper
    public static int fixParticleBrightness(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumSkyBlock type) {
        if(!world.provider.hasSkyLight() && type == EnumSkyBlock.SKY) return 0;
        if(pos.getY() < 0) pos = new BlockPos(pos.getX(), 0, pos.getZ());
        if(!world.isValid(pos)) return type.defaultLightValue;
        if(!world.getBlockState(pos).useNeighborBrightness()) {
            final FluidState fluidState = FluidState.get(pos);
            if(fluidState.isEmpty() || !fluidState.getState().useNeighborBrightness())
                return world.getLightFor(type, pos);
        }
        return Math.max(world.getLightFor(type, pos.up()),
                Math.max(world.getLightFor(type, pos.east()),
                        Math.max(world.getLightFor(type, pos.west()),
                                Math.max(world.getLightFor(type, pos.south()),
                                        world.getLightFor(type, pos.north())))));
    }

    //PluginParticleRain & PluginEntityRenderer
    @Nonnull
    public static AxisAlignedBB fixRainCollision(@Nonnull IBlockState here, @Nonnull World world, @Nonnull BlockPos pos) {
        final FluidState fluidState = getFluidState(world, pos, here);
        final AxisAlignedBB aabb = here.getBoundingBox(world, pos);
        //skip fluid check if none are present, or if it's a bad fluid
        if(fluidState.isEmpty() || !fluidState.isValid()) return aabb;
        final double fluidHeight = Math.max(
                getFluidFromState(here) == null ? aabb.maxY : 0,
                fluidState.getBlock().getFilledPercentage(world, pos));

        return new AxisAlignedBB(0, 0, 0, 0, fluidHeight, 0);
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

    //PluginTemplate
    public static void addFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull BlockPos transformedPos, @Nullable Block toIgnore, @Nonnull List<Pair<BlockPos, FluidState>> fluidStates) {
        final FluidState fluidState = FluidState.get(world, pos);
        if(!fluidState.isEmpty() && fluidState.getBlock() != toIgnore)
            fluidStates.add(Pair.of(transformedPos, fluidState));
    }

    //PluginTemplate
    public static void addFluidsToWorld(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull BlockPos size, @Nonnull PlacementSettings settings, int flags, @Nonnull List<Pair<BlockPos, FluidState>> fluidStates) {
        if(!fluidStates.isEmpty() && size.getX() > 0 && size.getZ() > 0) {
            for(Pair<BlockPos, FluidState> entry : fluidStates) {
                BlockPos transformedPos = Template.transformedBlockPos(settings, entry.getKey()).add(pos);
                setFluidState(world, transformedPos, null, entry.getValue(), false, true, flags);
            }
        }
    }

    //PluginTemplate
    public static int keepOldFlag(int blockFlags, boolean keepOldFluidStates) { return keepOldFluidStates ? blockFlags : (blockFlags | 32); }

    //PluginTemplate
    public static void readTemplate(@Nonnull Template template, @Nonnull NBTTagCompound compound, @Nonnull List<Pair<BlockPos, FluidState>> fluidStates) {
        fluidStates.clear();

        if(compound.hasKey("fluidStates", Constants.NBT.TAG_LIST)) {
            for(NBTBase nbtBase : compound.getTagList("fluidStates", Constants.NBT.TAG_COMPOUND)) {
                NBTTagCompound nbt = (NBTTagCompound)nbtBase;
                FluidState fluidState = FluidState.of(Block.getBlockFromName(nbt.getString("state")));

                if(!fluidState.isEmpty())
                    fluidStates.add(Pair.of(BlockPos.fromLong(nbt.getLong("pos")), fluidState));
            }
        }

        if(compound.hasKey("keepOldFluidStates", Constants.NBT.TAG_BYTE))
            ASMNatives.setKeepOldFluidStates(template, compound.getBoolean("keepOldFluidStates"));
    }

    //PluginTemplate
    public static void writeTemplate(@Nonnull NBTTagCompound compound, boolean keepOldFluidStates, @Nonnull List<Pair<BlockPos, FluidState>> fluidStates) {
        if(!fluidStates.isEmpty()) {
            final NBTTagList list = new NBTTagList();
            for(Pair<BlockPos, FluidState> entry : fluidStates) {
                NBTTagCompound nbt = new NBTTagCompound();
                nbt.setString("state", String.valueOf(entry.getValue().getBlock().getRegistryName()));
                nbt.setLong("pos", entry.getKey().toLong());
                list.appendTag(nbt);
            }

            compound.setTag("fluidStates", list);
        }

        compound.setBoolean("keepOldFluidStates", keepOldFluidStates);
    }

    //PluginWorld
    public static boolean isFlammableWithin(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb) {
        if(getFluidFromBlock(block) == FluidRegistry.LAVA) return Boolean.TRUE.equals(block.isAABBInsideLiquid(world, pos, bb));
        final FluidState fluidState = FluidState.get(world, pos); //handle possible lava FluidState
        return fluidState.getFluid() == FluidRegistry.LAVA && Boolean.TRUE.equals(fluidState.getBlock().isAABBInsideLiquid(world, pos, bb));
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
                        if(result != null) {
                            if(!result) continue;
                            return true;
                        }
                        else if(fluidState.getMaterial() == materialIn)
                            return true;
                    }
                }
            }
        }

        return false;
    }

    //PluginWorld & others
    public static IBlockState getFluidOrAir(World world, BlockPos pos) { return FluidState.get(world, pos).getState(); }

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
                    setFluidState(world, pos, newState, FluidState.EMPTY, false, false, flags);
            }

            //without the flag preserves FluidState / sets FluidState using oldState if it's a full fluid & if newState is fluidloggable
            else {
                //remove FluidState here, new state isn't fluidloggable
                if(!fluidState.isEmpty()) {
                    if(!isStateFluidloggable(newState, world, pos, fluidState.getFluid()))
                        setFluidState(world, pos, newState, FluidState.EMPTY, false, false, flags);
                    //ensure fluids are updated when the block here changes
                    else if(world.isAreaLoaded(pos, 1)) notifyFluids(world, pos, fluidState, true);
                }
                //save oldState as FluidState
                else {
                    final @Nullable Fluid fluid = getFluidFromState(oldState);
                    if(fluid != null && isFluidloggableFluid(oldState, world, pos) && isStateFluidloggable(newState, world, pos, fluid))
                        setFluidState(world, pos, newState, FluidState.of(fluid), false, false, flags);
                }
            }
        }

        return chunkState;
    }

    //PluginWorld
    @SuppressWarnings("ConstantConditions")
    @Nullable
    public static RayTraceResult rayTraceBlocks(@Nonnull World world, @Nonnull Vec3d vec, @Nonnull Vec3d end, boolean stopOnLiquid, boolean ignoreBlockWithoutBoundingBox, boolean returnLastUncollidableBlock) {
        if(Double.isNaN(vec.x) || Double.isNaN(vec.y) || Double.isNaN(vec.z) || Double.isNaN(end.x) || Double.isNaN(end.y) || Double.isNaN(end.z))
            return null;

        final int endX = MathHelper.floor(end.x);
        final int endY = MathHelper.floor(end.y);
        final int endZ = MathHelper.floor(end.z);
        int prevX = MathHelper.floor(vec.x);
        int prevY = MathHelper.floor(vec.y);
        int prevZ = MathHelper.floor(vec.z);

        BlockPos pos = new BlockPos(prevX, prevY, prevZ);
        @Nullable RayTraceResult result;

        //check FluidState
        if(stopOnLiquid) {
            final FluidState fluidState = FluidState.get(world, pos);
            if(!fluidState.isEmpty() && fluidState.getBlock().canCollideCheck(fluidState.getState(), true)) {
                result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                if(result != null) { return result; }
            }
        }

        IBlockState state = world.getBlockState(pos);
        if(state.getBlock().canCollideCheck(state, stopOnLiquid) && (stopOnLiquid && getFluidFromState(state) != null || !ignoreBlockWithoutBoundingBox || state.getCollisionBoundingBox(world, pos) != Block.NULL_AABB)) {
            result = state.collisionRayTrace(world, pos, vec, end);
            if(result != null) { return result; }
        }

        @Nullable RayTraceResult lastResult = null;
        for(int i = 200; i-- >= 0;) {
            if(Double.isNaN(vec.x) || Double.isNaN(vec.y) || Double.isNaN(vec.z))
                return null;

            if(prevX == endX && prevY == endY && prevZ == endZ)
                return returnLastUncollidableBlock ? lastResult : null;

            boolean flagX = true, flagY = true, flagZ = true;
            double x = 999, y = 999, z = 999;

            if(endX > prevX) x = prevX + 1;
            else if(endX < prevX) x = prevX;
            else flagX = false;

            if(endY > prevY) y = prevY + 1;
            else if(endY < prevY) y = prevY;
            else flagY = false;

            if(endZ > prevZ) z = prevZ + 1;
            else if(endZ < prevZ) z = prevZ;
            else flagZ = false;

            double coveredX = 999, coveredY = 999, coveredZ = 999;
            double distX = end.x - vec.x;
            double distY = end.y - vec.y;
            double distZ = end.z - vec.z;

            if(flagX) coveredX = (x - vec.x) / distX;
            if(flagY) coveredY = (y - vec.y) / distY;
            if(flagZ) coveredZ = (z - vec.z) / distZ;

            if(coveredX == -0) coveredX = -1.0E-4;
            if(coveredY == -0) coveredY = -1.0E-4;
            if(coveredZ == -0) coveredZ = -1.0E-4;

            //the general direction of the trace
            EnumFacing facing;

            if(coveredX < coveredY && coveredX < coveredZ) {
                facing = endX > prevX ? WEST : EAST;
                vec = new Vec3d(x, vec.y + distY * coveredX, vec.z + distZ * coveredX);
            }

            else if(coveredY < coveredZ) {
                facing = endY > prevY ? DOWN : UP;
                vec = new Vec3d(vec.x + distX * coveredY, y, vec.z + distZ * coveredY);
            }

            else {
                facing = endZ > prevZ ? NORTH : SOUTH;
                vec = new Vec3d(vec.x + distX * coveredZ, vec.y + distY * coveredZ, z);
            }

            prevX = MathHelper.floor(vec.x) - (facing == EAST  ? 1 : 0);
            prevY = MathHelper.floor(vec.y) - (facing == UP    ? 1 : 0);
            prevZ = MathHelper.floor(vec.z) - (facing == SOUTH ? 1 : 0);

            pos = new BlockPos(prevX, prevY, prevZ);

            //check FluidState
            if(stopOnLiquid) {
                FluidState fluidState = FluidState.get(world, pos);
                if(!fluidState.isEmpty()) {
                    if(fluidState.getBlock().canCollideCheck(fluidState.getState(), true)) {
                        result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                        if(result != null) return result;
                    }

                    else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
                }
            }

            state = world.getBlockState(pos);
            if(!ignoreBlockWithoutBoundingBox || state.getMaterial() == Material.PORTAL || stopOnLiquid && getFluidFromState(state) != null || state.getCollisionBoundingBox(world, pos) != Block.NULL_AABB) {
                if(state.getBlock().canCollideCheck(state, stopOnLiquid)) {
                    result = state.collisionRayTrace(world, pos, vec, end);
                    if(result != null) return result;
                }

                else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
            }
        }

        return returnLastUncollidableBlock ? lastResult : null;
    }

    //PluginWorldClient
    public static void randomFluidStateTick(@Nonnull World world, int x, int y, int z, int offset, @Nonnull Random random) {
        x += world.rand.nextInt(offset) - world.rand.nextInt(offset);
        y += world.rand.nextInt(offset) - world.rand.nextInt(offset);
        z += world.rand.nextInt(offset) - world.rand.nextInt(offset);

        final BlockPos pos = new BlockPos(x, y, z);
        final FluidState fluidState = FluidState.get(pos);
        if(!fluidState.isEmpty()) fluidState.getBlock().randomDisplayTick(fluidState.getState(), world, pos, random);
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

    //======
    //MODDED
    //======

    //PluginBetweenlands
    public static boolean fixBetweenlandsPlace(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState fluid, int flags, @Nonnull IFluidBlock block, @Nonnull IBlockState here) {
        if(isStateFluidloggable(here, world, pos, block.getFluid())) return setFluidState(world, pos, here, FluidState.of(block.getFluid()), true, true, flags);
        else return world.setBlockState(pos, fluid, flags);
    }

    //PluginBiomesOPlenty
    public static boolean canBOPFluidFlow(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumFacing facing) {
        final IBlockState here = world.getBlockState(pos);
        return here.getBlock().isReplaceable(world, pos)
                && canFluidFlow(world, pos, here, facing)
                && canFluidFlow(world, pos.offset(facing), world.getBlockState(pos.offset(facing)), facing.getOpposite());
    }

    //PluginBiomesOPlenty
    @Nonnull
    public static IBlockState getBOPFluidOrAir(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        final FluidState fluidState = getFluidState(world, pos);
        return fluidState.isEmpty() ? state : fluidState.getState();
    }
}
