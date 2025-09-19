/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.common.fluid.handler;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.IFluidNeighborInfo;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.ISpecializedFluidNeighborInfo;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.World;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.BlockFluidFinite;
import net.minecraftforge.fml.common.FMLCommonHandler;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.Optional;
import java.util.function.ToDoubleFunction;

/**
 * Implemented by {@link net.minecraft.block.BlockLiquid BlockLiquid} and {@link net.minecraftforge.fluids.BlockFluidBase BlockFluidBase} at runtime.
 * @author jbred
 *
 */
public final class FluidExtendedStateHandler
{
    @Nonnull
    public static IBlockState getExtendedState(@Nonnull final IBlockState renderState, @Nonnull final ISpecializedFluidNeighborInfo neighborInfo, @Nonnull final ToDoubleFunction<ISpecializedFluidNeighborInfo> flowDirection) {
        // should never pass, but let's be safe
        if(!(renderState instanceof IExtendedBlockState) || !neighborInfo.getOrigin().isValid()) return renderState;

        // convert to special state for performance
        @Nonnull final FluidExtendedBlockState state = new FluidExtendedBlockState((IExtendedBlockState)renderState);

        // corner height variables
        @Nonnull final float[][] heights = new float[4][4];
        @Nonnull final float[][] corners = new float[2][2];

        // if this fluid is connected to another above, set all corner heights to 1
        if((heights[1][1] = isConnectedVertical(neighborInfo, 1, 1) ? 1 : getRenderLevel(neighborInfo, 1, 1, 1)) == 1)
            for(int i = 0; i < 2; i++) corners[i] = new float[] {1, 1};

        // no fluid block above this
        else {
            // sync middle values
            heights[2][2] = heights[1][2] = heights[2][1] = heights[1][1];

            // calculate fluid height values
            for(int xiH = 0; xiH < 4; xiH++) for(int ziH = 0; ziH < 4; ziH++) if(xiH == 0 || xiH == 3 || ziH == 0 || ziH == 3)
                heights[xiH][ziH] = getFluidHeightForRender(neighborInfo, xiH, ziH);

            // calculate fluid corner height values, by finding the average of gathered heights for each corner
            for(int xiH = 0; xiH < 4; xiH += 2) for(int ziH = 0; ziH < 4; ziH += 2) corners[xiH >> 1][ziH >> 1] = getFluidHeightAverage(
                    neighborInfo.getOrigin().getQuantaFraction(),
                    heights[xiH][ziH],
                    heights[xiH][ziH + 1],
                    heights[xiH + 1][ziH],
                    heights[xiH + 1][ziH + 1]);
        }

        // only calculate client-side props if state is being gathered for rendering
        boolean calcFlowDirection = false;
        if(FMLCommonHandler.instance().getSide().isClient() && MinecraftForgeClient.getRenderLayer() != null) {
            // don't calculate quads for sides that won't end up rendering anyway, this results in much better performance during rendering
            for(@Nonnull final EnumFacing side : EnumFacing.VALUES) state.shouldSideBeRenderedCache[side.getIndex()] = shouldFluidSideBeRendered(neighborInfo, side);

            // side overlays, skipped if there's no overlay texture
            if(neighborInfo.getOrigin().getFluid().getOverlay() != null) {
                for(int i = 0; i < 4; i++) {
                    if(state.shouldSideBeRenderedCache[EnumFacing.HORIZONTALS[i].getIndex()]) {
                        @Nonnull final Vec3i direction = EnumFacing.HORIZONTALS[i].getDirectionVec();
                        state.sideOverlays[i] = !neighborInfo.canFluidFlowI(direction.getX() + 1, 1, direction.getZ() + 1, EnumFacing.HORIZONTALS[i].getOpposite());
                    }
                }
            }

            // apply vanilla's check for under surface face render (issue#202)
            if(heights[1][1] != 1) {
                if(!neighborInfo.canFluidFlowI(1, 1, 1, neighborInfo.getOrigin().getUpDensityFace())) state.renderUnder = true;
                else for(int xi = 0; xi < 3; xi++) for(int zi = 0; zi < 3; zi++) {
                    if(neighborInfo.canFluidFlowI(xi, 2, zi, neighborInfo.getOrigin().getDownDensityFace()) && !neighborInfo.isCompatibleFluidI(xi, 2, zi)) {
                        state.renderUnder = true;
                        break;
                    }
                }
            }

            // fix possible top z fighting
            if(state.shouldSideBeRenderedCache[neighborInfo.getOrigin().getUpDensityFace().getIndex()]) {
                if(corners[0][0] == 1) corners[0][0] = 1 - 1e-3f;
                if(corners[0][1] == 1) corners[0][1] = 1 - 1e-3f;
                if(corners[1][0] == 1) corners[1][0] = 1 - 1e-3f;
                if(corners[1][1] == 1) corners[1][1] = 1 - 1e-3f;
                calcFlowDirection = true;
            }
        }

        // set remaining properties
        state.flowDirection = calcFlowDirection ? (float)flowDirection.applyAsDouble(neighborInfo) : -1000f;
        state.levelCorners[0] = corners[0][0];
        state.levelCorners[1] = corners[0][1];
        state.levelCorners[2] = corners[1][1];
        state.levelCorners[3] = corners[1][0];

        return state;
    }

    static float getFluidHeightForRender(@Nonnull final ISpecializedFluidNeighborInfo neighborInfo, final int xiH, final int ziH) {
        final float connection = getHorizontalConnection(neighborInfo, xiH, 1, ziH);

        // check for vertical fluid
        if(connection != -1 && isConnectedVertical(neighborInfo, xiH > 1 ? xiH - 1 : xiH, ziH > 1 ? ziH - 1 : ziH)
        || isConnectedVertical(neighborInfo, 1, 1) && getHorizontalConnection(neighborInfo, xiH, 2, ziH) != -1) return 1;

        // default
        else return connection;
    }

    static float getHorizontalConnection(@Nonnull final ISpecializedFluidNeighborInfo neighborInfo, final int xiH, final int yi, final int ziH) {
        @Nonnull final EnumFacing xSide = xiH > 1 ? EnumFacing.EAST : EnumFacing.WEST;
        @Nonnull final EnumFacing zSide = ziH > 1 ? EnumFacing.SOUTH : EnumFacing.NORTH;
        final int xi = xiH > 1 ? xiH - 1 : xiH, zi = ziH > 1 ? ziH - 1 : ziH;

        // x-axis
        if(zi == 1) {
            if(!neighborInfo.canFluidFlowI(1, yi, 1, xSide) || !canFlowOrReplaceable(neighborInfo, xi, yi, zi, 1, xSide.getOpposite())) {
                final int zc = (ziH - 1) << 1;
                if(!neighborInfo.canFluidFlowI(1, yi, 1, zSide) || !canFlowOrReplaceable(neighborInfo, 1, yi, zc, 1, zSide.getOpposite()) ||
                   !canFlowOrReplaceable(neighborInfo, 1, yi, zc, 1, xSide) || !canFlowOrReplaceable(neighborInfo, xi, yi, zc, 2, xSide.getOpposite()) ||
                   !canFlowOrReplaceable(neighborInfo, xi, yi, zc, 2, zSide.getOpposite()) || !canFlowOrReplaceable(neighborInfo, xi, yi, zi, 3, zSide)) return -1;
            }

            return neighborInfo.isCompatibleFluidI(xi, yi, zi) ? getRenderLevel(neighborInfo, xi, yi, zi) : 0;
        }

        // z-axis
        else if(xi == 1) {
            if(!neighborInfo.canFluidFlowI(1, yi, 1, zSide) || !canFlowOrReplaceable(neighborInfo, xi, yi, zi, 1, zSide.getOpposite())) {
                final int xc = (xiH - 1) << 1;
                if(!neighborInfo.canFluidFlowI(1, yi, 1, xSide) || !canFlowOrReplaceable(neighborInfo, xc, yi, 1, 1, xSide.getOpposite()) ||
                   !canFlowOrReplaceable(neighborInfo, xc, yi, 1, 1, zSide) || !canFlowOrReplaceable(neighborInfo, xc, yi, zi, 2, zSide.getOpposite()) ||
                   !canFlowOrReplaceable(neighborInfo, xc, yi, zi, 2, xSide.getOpposite()) || !canFlowOrReplaceable(neighborInfo, xi, yi, zi, 3, xSide)) return -1;
            }

            return neighborInfo.isCompatibleFluidI(xi, yi, zi) ? getRenderLevel(neighborInfo, xi, yi, zi) : 0;
        }

        // corner
        else return
            (neighborInfo.canFluidFlowI(1, yi, 1, xSide) && canFlowOrReplaceable(neighborInfo, xi, yi, 1, 1, xSide.getOpposite())
            && canFlowOrReplaceable(neighborInfo, xi, yi, 1, 1, zSide) && canFlowOrReplaceable(neighborInfo, xi, yi, zi, 2, zSide.getOpposite())

            || neighborInfo.canFluidFlowI(1, yi, 1, zSide) && canFlowOrReplaceable(neighborInfo, 1, yi, zi, 1, zSide.getOpposite())
            && canFlowOrReplaceable(neighborInfo, 1, yi, zi, 1, xSide) && canFlowOrReplaceable(neighborInfo, xi, yi, zi, 2, xSide.getOpposite()))

            ? neighborInfo.isCompatibleFluidI(xi, yi, zi) ? getRenderLevel(neighborInfo, xi, yi, zi) : 0 : -1;
    }

    static boolean canFlowOrReplaceable(@Nonnull final ISpecializedFluidNeighborInfo neighborInfo, final int xi, final int yi, final int zi, final int fallbackDist, @Nonnull final EnumFacing side) {
        @Nonnull final FluidState dummyState = findLargestConnected(neighborInfo, xi, yi, zi, fallbackDist);
        return dummyState == FluidState.EMPTY || neighborInfo.isReplaceableI(xi, yi, zi, dummyState, side, true, false) || neighborInfo.canFluidFlowI(xi, yi, zi, side) &&
                (neighborInfo.isCompatibleFluidI(xi, yi, zi) || neighborInfo.isFluidloggableI(xi, yi, zi, dummyState, side, true, false));
    }

    @Nonnull
    static FluidState findLargestConnected(@Nonnull final ISpecializedFluidNeighborInfo neighborInfo, final int xi, final int yi, final int zi, final int fallbackDist) {
        if(neighborInfo.isCompatibleFluidI(xi, yi, zi)) return neighborInfo.getFluidStateI(xi, yi, zi);
        @Nonnull final World world = neighborInfo.getCache().getWorld();

        final int flowCost = neighborInfo.getOrigin().getFlowCost(world);
        final boolean isFinite = neighborInfo.getOrigin().getBlock() instanceof BlockFluidFinite;

        @Nonnull FluidState largest = FluidState.EMPTY;
        for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
            if(neighborInfo.canFluidFlowI(xi, yi, zi, side)) {
                final int xo = side.getXOffset() + xi, zo = side.getZOffset() + zi;
                if(xo >= 0 && xo < 3 && zo >= 0 && zo < 3 && (xo == 1 || zo == 1)
                && neighborInfo.isCompatibleFluidI(xo, yi, zo)
                && neighborInfo.canFluidFlowI(xo, yi, zo, side.getOpposite())) {
                    @Nonnull final FluidState fluidState = neighborInfo.getFluidStateI(xo, yi, zo);
                    if(largest == FluidState.EMPTY || (isFinite ? largest.getLevel() < fluidState.getLevel()
                    : largest.getWrappedLevel(world) > fluidState.getWrappedLevel(world))) largest = fluidState;
                    if(largest.isSource()) return largest.addLevel(flowCost);
                }
            }
        }

        return largest == FluidState.EMPTY ? neighborInfo.getOrigin().addLevel(fallbackDist * flowCost) : largest.addLevel(flowCost);
        //return neighborInfo.getOrigin().asSource();
    }

    static float getRenderLevel(@Nonnull final IFluidNeighborInfo neighborInfo, final int xi, final int yi, final int zi) {
        @Nonnull final FluidState fluidState = neighborInfo.getFluidStateI(xi, yi, zi);
        if(fluidState.getBlock() instanceof BlockFluidBase) return fluidState.getMetadata() == ((BlockFluidBase)fluidState.getBlock()).getMaxRenderHeightMeta() ? fluidState.getQuantaFraction() : fluidState.getHeight();
        else return fluidState.isSource() ? fluidState.getQuantaFraction() : fluidState.getHeight();
    }

    @Nonnull
    static FluidState addLevel(@Nonnull final IFluidNeighborInfo neighborInfo, final int levelToAdd) {
        @Nonnull final FluidState origin = neighborInfo.getOrigin();
        return origin.withLevel(origin.getBlock() instanceof BlockLiquid && origin.getLevel() >= 8 ? levelToAdd : Math.min(origin.getLevel() + levelToAdd, origin.getQuantaPerBlock() - 1));
    }

    // return true if the fluid at the pos is physically connected vertically to the origin fluid
    static boolean isConnectedVertical(@Nonnull final IFluidNeighborInfo neighborInfo, final int xi, final int zi) {
        return neighborInfo.isCompatibleFluidI(xi, 1, zi) && neighborInfo.isCompatibleFluidI(xi, 2, zi)
                && neighborInfo.canFluidFlowI(xi, 1, zi, neighborInfo.getOrigin().getUpDensityFace())
                && neighborInfo.canFluidFlowI(xi, 2, zi, neighborInfo.getOrigin().getDownDensityFace());
    }

    // copied from BlockFluidBase
    static float getFluidHeightAverage(final float quantaFraction, @Nonnull final float... heights) {
        float total = 0;
        int count = 0;

        for(final float height : heights) {
            if(height == 1) return 1; // vertical fluid
            else if(height >= quantaFraction) {
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

    static boolean shouldFluidSideBeRendered(@Nonnull final IFluidNeighborInfo neighborInfo, @Nonnull final EnumFacing side) {
        if(!neighborInfo.canFluidFlowI(1, 1, 1, side)) return true;

        @Nonnull final Vec3i vec = side.getDirectionVec();
        final int xi = vec.getX() + 1, yi = vec.getY() * -neighborInfo.getOrigin().getDensityDir() + 1, zi = vec.getZ() + 1;
        @Nonnull final IBlockState neighbor = neighborInfo.getBlockStateI(xi, yi, zi);

        // this check exists for mods like coral reef that don't have proper block sides
        if(FluidloggedUtils.isCompatibleFluid(neighborInfo.getOrigin().getFluid(), FluidloggedUtils.getFluidFromState(neighbor))) return false;
        else if(vec.getY() != -neighborInfo.getOrigin().getDensityDir() && neighbor.doesSideBlockRendering(neighborInfo.getCache(), neighborInfo.getPosIB(xi, yi, zi), side.getOpposite())) return false;
        else return !neighborInfo.canFluidFlowI(xi, yi, zi, side.getOpposite()) || !neighborInfo.isCompatibleFluidI(xi, yi, zi);
    }

    // faster version of the default IExtendedBlockState but with hardcoded properties, used exclusively for rendering & collision logic
    public static final class FluidExtendedBlockState extends BlockStateContainer.StateImplementation implements IExtendedBlockState
    {
        public Float flowDirection;
        public final Float[] levelCorners = new Float[4];
        public final Boolean[] sideOverlays = new Boolean[4];

        public final boolean[] shouldSideBeRenderedCache = new boolean[6];
        public boolean renderUnder;

        @Nonnull
        private final IExtendedBlockState parent;
        public FluidExtendedBlockState(@Nonnull final IExtendedBlockState parentIn) {
            super(parentIn.getBlock(), parentIn.getProperties());
            parent = parentIn;
        }

        @SuppressWarnings("unchecked")
        @Nonnull
        @Override
        public <V> V getValue(@Nonnull final IUnlistedProperty<V> property) {
            if(property == BlockFluidBase.LEVEL_CORNERS[0]) return (V)levelCorners[0];
            else if(property == BlockFluidBase.LEVEL_CORNERS[1]) return (V)levelCorners[1];
            else if(property == BlockFluidBase.LEVEL_CORNERS[2]) return (V)levelCorners[2];
            else if(property == BlockFluidBase.LEVEL_CORNERS[3]) return (V)levelCorners[3];
            else if(property == BlockFluidBase.FLOW_DIRECTION) return (V)flowDirection;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[0]) return (V)sideOverlays[0];
            else if(property == BlockFluidBase.SIDE_OVERLAYS[1]) return (V)sideOverlays[1];
            else if(property == BlockFluidBase.SIDE_OVERLAYS[2]) return (V)sideOverlays[2];
            else if(property == BlockFluidBase.SIDE_OVERLAYS[3]) return (V)sideOverlays[3];

            // a block is using special unlisted properties
            else return parent.getValue(property);
        }

        @Nonnull
        @Override
        public <V> IExtendedBlockState withProperty(@Nonnull final IUnlistedProperty<V> property, @Nonnull final V value) {
            if(property == BlockFluidBase.LEVEL_CORNERS[0]) levelCorners[0] = (Float)value;
            else if(property == BlockFluidBase.LEVEL_CORNERS[1]) levelCorners[1] = (Float)value;
            else if(property == BlockFluidBase.LEVEL_CORNERS[2]) levelCorners[2] = (Float)value;
            else if(property == BlockFluidBase.LEVEL_CORNERS[3]) levelCorners[3] = (Float)value;
            else if(property == BlockFluidBase.FLOW_DIRECTION) flowDirection = (Float)value;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[0]) sideOverlays[0] = (Boolean)value;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[1]) sideOverlays[1] = (Boolean)value;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[2]) sideOverlays[2] = (Boolean)value;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[3]) sideOverlays[3] = (Boolean)value;

            // a block is using special unlisted properties
            else {
                @Nonnull final FluidExtendedBlockState newState = new FluidExtendedBlockState(parent.withProperty(property, value));
                newState.flowDirection = flowDirection;
                newState.renderUnder = renderUnder;

                System.arraycopy(levelCorners, 0, newState.levelCorners, 0, 4);
                System.arraycopy(sideOverlays, 0, newState.sideOverlays, 0, 4);
                System.arraycopy(shouldSideBeRenderedCache, 0, newState.shouldSideBeRenderedCache, 0, 6);
                return newState;
            }

            return this;
        }

        @Nonnull
        @Override
        public ImmutableMap<IUnlistedProperty<?>, Optional<?>> getUnlistedProperties() { return parent.getUnlistedProperties(); }

        @Nonnull
        @Override
        public Collection<IUnlistedProperty<?>> getUnlistedNames() { return parent.getUnlistedNames(); }

        @Nonnull
        @Override
        public IBlockState getClean() { return parent.getClean(); }
    }
}
