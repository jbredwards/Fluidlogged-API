/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.client.model;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginBlockFluidBase;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidExtendedStateHandler;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.block.model.ItemCameraTransforms;
import net.minecraft.client.renderer.block.model.ItemOverrideList;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.renderer.vertex.VertexFormat;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3i;
import net.minecraftforge.client.model.PerspectiveMapWrapper;
import net.minecraftforge.client.model.pipeline.IVertexConsumer;
import net.minecraftforge.client.model.pipeline.TRSRTransformer;
import net.minecraftforge.client.model.pipeline.UnpackedBakedQuad;
import net.minecraftforge.common.model.IModelState;
import net.minecraftforge.common.model.TRSRTransformation;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.vecmath.Matrix4f;
import java.util.*;
import java.util.function.Function;

/**
 * An implementation of Forge's baked fluid model that fixes issue#176
 * @author jbred
 *
 */
@SideOnly(Side.CLIENT)
public class BakedModelFluid implements IBakedModel
{
    @Nonnull protected static final Logger LOGGER = LogManager.getFormatterLogger("Fluidlogged API Renderer");

    protected static final int[] WSEN = { 1, 0, 3, 2 }; // [W, S, E, N]
    protected static final int[] x = { 0, 0, 1, 1 };
    protected static final int[] z = { 0, 1, 1, 0 };
    protected static final float eps = 1e-3f;

    @Nullable protected final TRSRTransformation transformation;
    @Nonnull protected final ImmutableMap<ItemCameraTransforms.TransformType, TRSRTransformation> transforms;
    protected final boolean hasTransform;

    @Nonnull protected final VertexFormat format;
    @Nonnull protected final TextureAtlasSprite still, flowing;
    @Nullable protected final TextureAtlasSprite overlay;

    protected final float[] argb = new float[4];
    protected final boolean gas;

    @Nonnull protected final Function<SerializedProps, List<BakedQuad>[]> blockQuads;
    @Nullable protected List<BakedQuad> itemQuads;

    public BakedModelFluid(@Nonnull Fluid fluidIn, @Nonnull IModelState stateIn, @Nonnull VertexFormat formatIn, @Nonnull Function<ResourceLocation, TextureAtlasSprite> textureGetterIn) { this(fluidIn, fluidIn.getStill(), fluidIn.getFlowing(), stateIn, formatIn, textureGetterIn); }
    public BakedModelFluid(@Nonnull Fluid fluidIn, @Nonnull ResourceLocation stillIn, @Nonnull ResourceLocation flowingIn, @Nonnull IModelState stateIn, @Nonnull VertexFormat formatIn, @Nonnull Function<ResourceLocation, TextureAtlasSprite> textureGetterIn) {
        transformation = stateIn.apply(Optional.empty()).orElse(null);
        transforms = PerspectiveMapWrapper.getTransforms(stateIn);
        hasTransform = transformation != null && !transformation.isIdentity();

        format = formatIn;
        still = textureGetterIn.apply(stillIn);
        flowing = textureGetterIn.apply(flowingIn);
        overlay = fluidIn.getOverlay() != null ? textureGetterIn.apply(fluidIn.getOverlay()) : null;

        final int color = fluidIn.getColor();
        argb[0] = ((color >> 24) & 0xFF) / 255f;
        argb[1] = ((color >> 16) & 0xFF) / 255f;
        argb[2] = ((color >> 8)  & 0xFF) / 255f;
        argb[3] = ( color        & 0xFF) / 255f;
        gas = fluidIn.isLighterThanAir();

        // blockQuads = this::buildQuads; // for testing
        blockQuads = CacheBuilder.newBuilder().maximumSize(512).build(CacheLoader.from(this::buildQuads));
    }

    @Nonnull
    protected List<BakedQuad>[] buildQuads(@Nonnull final SerializedProps key) {
        final List<BakedQuad>[] ret = new List[6];

        final float[] levelCorners = key.levelCorners();
        final boolean[] sideOverlays = key.sideOverlays();
        // final boolean[] shouldSideBeRenderedCache = key.shouldSideBeRenderedCache();

        for(@Nonnull final EnumFacing side : EnumFacing.VALUES) {
            if(key.sideToBuild != null && key.sideToBuild != side) ret[side.getIndex()] = Collections.emptyList();

            // if(!shouldSideBeRenderedCache[side.getIndex()]) sideQuads[side.getIndex()] = Collections.emptyList(); // if side cannot render, don't bother calculating quads
            else if(side.getAxis() == EnumFacing.Axis.Y) {
                // top
                final EnumFacing top = gas ? EnumFacing.DOWN : EnumFacing.UP;
                if(side == top) {
                    // y levels
                    final float[] y = new float[4];
                    for(int i = 0; i < 4; i++) y[i] = gas ? 1 - levelCorners[i] : levelCorners[i];

                    // flow
                    final int flowDirection = key.flowDirection();
                    final boolean isFlowing = flowDirection > -1000;
                    final float flow = isFlowing ? (float)Math.toRadians(flowDirection) : 0;

                    final TextureAtlasSprite topSprite = isFlowing ? flowing : still;
                    final float scale = isFlowing ? 4 : 8;

                    final float c = MathHelper.cos(flow) * scale;
                    final float s = MathHelper.sin(flow) * scale;

                    // base uv offset for flow direction
                    final VertexParameter uv = i -> c * ((x[i] << 1) - 1) + s * ((z[i] << 1) - 1);
                    final VertexParameter topX = i -> x[i];
                    final VertexParameter topY = i -> y[i];
                    final VertexParameter topZ = i -> z[i];
                    final VertexParameter topU = i -> 8 + uv.get(i);
                    final VertexParameter topV = i -> 8 + uv.get((i + 1) & 3);

                    final List<BakedQuad> quads = new ArrayList<>(1);
                    quads.add(buildQuad(top, topSprite, gas, false, topX, topY, topZ, topU, topV));
                    if(key.renderUnder()) quads.add(buildQuad(top, topSprite, !gas, true, topX, topY, topZ, topU, topV));

                    ret[side.getIndex()] = Collections.unmodifiableList(quads);
                }

                // bottom
                else ret[side.getIndex()] = Collections.singletonList(buildQuad(side, still, gas, false, i -> z[i], i -> gas ? 1 - eps : eps, i -> x[i], i -> z[i] << 4, i -> x[i] << 4));
            }

            // sides
            else {
                final boolean useOverlay = overlay != null && sideOverlays[side.getHorizontalIndex()];
                final int si = WSEN[side.getHorizontalIndex()]; // local var for lambda capture

                final float[] y = new float[4];
                for(int i = 0; i < 4; i++) y[i] = gas ? 1 - levelCorners[i] : levelCorners[i];

                final VertexParameter sideX = j -> fixTextureFightingX(x[(si + x[j]) & 3], side);
                final VertexParameter sideY = j -> z[j] == 0 ? (gas ? 1 : 0) : y[(si + x[j]) & 3];
                final VertexParameter sideZ = j -> fixTextureFightingZ(z[(si + x[j]) & 3], side);
                final VertexParameter sideU = j -> x[j] << 3;
                final VertexParameter sideV = j -> (gas ? sideY.get(j) : 1 - sideY.get(j)) * 8;

                final List<BakedQuad> quads = new ArrayList<>(1);
                if(!useOverlay) quads.add(buildQuad(side, flowing, gas, true, sideX, sideY, sideZ, sideU, sideV));
                quads.add(buildQuad(side, useOverlay ? overlay : flowing, !gas, false, sideX, sideY, sideZ, sideU, sideV));

                ret[side.getIndex()] = Collections.unmodifiableList(quads);
            }
        }

        return ret;
    }

    @Nonnull
    protected BakedQuad buildQuad(@Nonnull EnumFacing side, @Nonnull TextureAtlasSprite texture, boolean flip, boolean offset, @Nonnull VertexParameter x, @Nonnull VertexParameter y, @Nonnull VertexParameter z, @Nonnull VertexParameter u, @Nonnull VertexParameter v) {
        final UnpackedBakedQuad.Builder builder = new UnpackedBakedQuad.Builder(format);
        final IVertexConsumer consumer = hasTransform ? new TRSRTransformer(builder, transformation) : builder;

        consumer.setQuadOrientation(side);
        consumer.setTexture(texture);
        consumer.setQuadTint(0);

        for(int i = 0; i < 4; i++) {
            final int vertex = flip ? 3 - i : i;
            putVertex(consumer, side, offset, x.get(vertex), y.get(vertex), z.get(vertex), texture.getInterpolatedU(u.get(vertex)), texture.getInterpolatedV(v.get(vertex)));
        }

        return builder.build();
    }

    protected void putVertex(@Nonnull IVertexConsumer consumer, @Nonnull EnumFacing side, boolean offset, float x, float y, float z, float u, float v) {
        for(int element = 0; element < format.getElementCount(); element++) {
            switch(format.getElement(element).getUsage()) {
                case POSITION:
                    if(offset) {
                        final Vec3i vec = side.getDirectionVec();
                        float dx = vec.getX() * eps;
                        float dy = vec.getY() * eps;
                        float dz = vec.getZ() * eps;
                        consumer.put(element, x - dx, y - dy, z - dz, 1);
                    }

                    else consumer.put(element, x, y, z, 1);
                    break;

                case COLOR:
                    consumer.put(element, argb[1], argb[2], argb[3], argb[0]);
                    break;

                case NORMAL:
                    final Vec3i vec = side.getDirectionVec();
                    consumer.put(element, vec.getX(), vec.getY(), vec.getZ(), 0);
                    break;

                case UV:
                    if(format.getElement(element).getIndex() == 0) {
                        consumer.put(element, u, v, 0, 1);
                        break;
                    }

                default:
                    consumer.put(element);
                    break;
            }
        }
    }

    @Override
    public boolean isAmbientOcclusion() { return true; }

    @Override
    public boolean isGui3d() { return false; }

    @Override
    public boolean isBuiltInRenderer() { return false; }

    @Nonnull
    @Override
    public TextureAtlasSprite getParticleTexture() { return still; }

    @Nonnull
    @Override
    public ItemOverrideList getOverrides() { return ItemOverrideList.NONE; }

    @Nonnull
    @Override
    public Pair<? extends IBakedModel, Matrix4f> handlePerspective(@Nonnull ItemCameraTransforms.TransformType cameraTransformType) {
        return PerspectiveMapWrapper.handlePerspective(this, transforms, cameraTransformType);
    }

    @Nonnull
    @Override
    public List<BakedQuad> getQuads(@Nullable IBlockState state, @Nullable EnumFacing side, long rand) {
        if(side == null || !(state instanceof IExtendedBlockState)) return state == null && side == EnumFacing.SOUTH ? itemQuads == null ? itemQuads = Collections.singletonList(buildQuad(EnumFacing.UP, still, false, false, i -> z[i], i -> x[i], i -> 0, i -> z[i] << 4, i -> x[i] << 4)) : itemQuads : Collections.emptyList();
        else return blockQuads.apply(new SerializedProps(null, state instanceof FluidExtendedStateHandler.FluidExtendedBlockState ? (FluidExtendedStateHandler.FluidExtendedBlockState)state : createFluidExtendedState((IExtendedBlockState)state)))[side.getIndex()];
    }

    // mod is passing in default state (shouldn't happen), assuming default properties for default state...
    @Nonnull
    protected static FluidExtendedStateHandler.FluidExtendedBlockState createFluidExtendedState(@Nonnull final IExtendedBlockState stateIn) {
        LOGGER.error("Either a mod is trying to render a fluid without calling Block.getExtendedState, or the fluid block overrides Block.getExtendedState! Assuming default properties for: \"" + stateIn + '"');

        @Nonnull final FluidExtendedStateHandler.FluidExtendedBlockState state = new FluidExtendedStateHandler.FluidExtendedBlockState(stateIn);
        for(int i = 0; i < 4; i++) {
            state.sideOverlays[i] = stateIn.getValue(BlockFluidBase.SIDE_OVERLAYS[i]);
            final float corner = stateIn.getValue(BlockFluidBase.LEVEL_CORNERS[i]);
            state.levelCorners[i] = corner == 0 ? stateIn.getBlock() instanceof PluginBlockFluidBase.Accessor ? ((PluginBlockFluidBase.Accessor)stateIn.getBlock()).getQuantaFraction_Public() : 8f/9 : corner;
        }

        // apply forge's check for under surface face rendering (causes issue#202)
        for(int i = 0; i < 4; i++) {
            if(state.levelCorners[i] < 1) {
                state.renderUnder = true;
                break;
            }
        }

        Arrays.fill(state.shouldSideBeRenderedCache, true); // unchecked sides, calculate quads for all sides
        state.flowDirection = stateIn.getValue(BlockFluidBase.FLOW_DIRECTION);
        return state;
    }

    public static float fixTextureFightingX(float old, @Nonnull EnumFacing side) {
        return side.getAxis() == EnumFacing.Axis.Z ? old : old == 1 ? 1 - eps : eps;
    }

    public static float fixTextureFightingZ(float old, @Nonnull EnumFacing side) {
        return side.getAxis() == EnumFacing.Axis.X ? old : old == 1 ? 1 - eps : eps;
    }

    // maps vertex index to parameter value
    @FunctionalInterface
    public interface VertexParameter { float get(int index); }
    public static class SerializedProps implements Comparable<SerializedProps>
    {
        @Nonnull
        public final int[] cornerHeights = new int[4];
        public final int serializedProps;

        @Nullable
        public final EnumFacing sideToBuild; // only not null if the cache isn't being used (for testing purposes)
        public SerializedProps(@Nullable final EnumFacing side, @Nonnull final FluidExtendedStateHandler.FluidExtendedBlockState state) {
            serializedProps = (MathHelper.clamp((int)Math.round(Math.toDegrees(state.flowDirection)), -1000, 1000) + 1024)
                    | (state.renderUnder ? 0b100000000000 : 0)
                    | (Boolean.TRUE.equals(state.sideOverlays[0]) ? 0b1000000000000 : 0)
                    | (Boolean.TRUE.equals(state.sideOverlays[1]) ? 0b10000000000000 : 0)
                    | (Boolean.TRUE.equals(state.sideOverlays[2]) ? 0b100000000000000 : 0)
                    | (Boolean.TRUE.equals(state.sideOverlays[3]) ? 0b1000000000000000 : 0);
                    // | (Boolean.TRUE.equals(state.shouldSideBeRenderedCache[0]) ? 0b10000000000000000 : 0)
                    // | (Boolean.TRUE.equals(state.shouldSideBeRenderedCache[1]) ? 0b100000000000000000 : 0)
                    // | (Boolean.TRUE.equals(state.shouldSideBeRenderedCache[2]) ? 0b1000000000000000000 : 0)
                    // | (Boolean.TRUE.equals(state.shouldSideBeRenderedCache[3]) ? 0b10000000000000000000 : 0)
                    // | (Boolean.TRUE.equals(state.shouldSideBeRenderedCache[4]) ? 0b100000000000000000000 : 0)
                    // | (Boolean.TRUE.equals(state.shouldSideBeRenderedCache[5]) ? 0b1000000000000000000000 : 0);
            cornerHeights[0] = Float.floatToRawIntBits(state.levelCorners[0]);
            cornerHeights[1] = Float.floatToRawIntBits(state.levelCorners[1]);
            cornerHeights[2] = Float.floatToRawIntBits(state.levelCorners[2]);
            cornerHeights[3] = Float.floatToRawIntBits(state.levelCorners[3]);
            sideToBuild = side;
        }

        public int flowDirection() { return (serializedProps & 2047) - 1024; }
        public boolean renderUnder() { return (serializedProps & 0b100000000000) != 0; }

        @Nonnull
        public float[] levelCorners() {
            @Nonnull final float[] ret = new float[4];
            ret[0] = Float.intBitsToFloat(cornerHeights[0]);
            ret[1] = Float.intBitsToFloat(cornerHeights[1]);
            ret[2] = Float.intBitsToFloat(cornerHeights[2]);
            ret[3] = Float.intBitsToFloat(cornerHeights[3]);
            return ret;
        }

        @Nonnull
        public boolean[] sideOverlays() {
            @Nonnull final boolean[] ret = new boolean[4];
            ret[0] = (serializedProps & 0b1000000000000) != 0;
            ret[1] = (serializedProps & 0b10000000000000) != 0;
            ret[2] = (serializedProps & 0b100000000000000) != 0;
            ret[3] = (serializedProps & 0b1000000000000000) != 0;
            return ret;
        }

        @Nonnull
        public boolean[] shouldSideBeRenderedCache() {
            @Nonnull final boolean[] ret = new boolean[6];
            ret[0] = (serializedProps & 0b10000000000000000) != 0;
            ret[1] = (serializedProps & 0b100000000000000000) != 0;
            ret[2] = (serializedProps & 0b1000000000000000000) != 0;
            ret[3] = (serializedProps & 0b10000000000000000000) != 0;
            ret[4] = (serializedProps & 0b100000000000000000000) != 0;
            ret[5] = (serializedProps & 0b1000000000000000000000) != 0;
            return ret;
        }

        @Override
        public boolean equals(@Nullable final Object o) {
            if(this == o) return true;
            else if(o == null || getClass() != o.getClass()) return false;
            else return serializedProps == ((SerializedProps)o).serializedProps && Arrays.equals(cornerHeights, ((SerializedProps)o).cornerHeights);
        }

        @Override
        public int hashCode() {
            int result = Arrays.hashCode(cornerHeights);
            result = 31 * result + Integer.hashCode(serializedProps);
            return result;
        }

        @Override
        public int compareTo(@Nonnull final SerializedProps o) {
            return new CompareToBuilder().append(serializedProps, o.serializedProps).append(cornerHeights, o.cornerHeights).toComparison();
        }
    }
}
