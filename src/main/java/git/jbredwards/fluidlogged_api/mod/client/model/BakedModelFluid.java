package git.jbredwards.fluidlogged_api.mod.client.model;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginBlockFluidBase;
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
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.vecmath.Matrix4f;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

/**
 * An implementation of Forge's baked fluid model that doesn't use a cache (fixes issue#176)
 * @author jbred
 *
 */
@SideOnly(Side.CLIENT)
public class BakedModelFluid implements IBakedModel
{
    protected static final int[] WSEN = { 1, 0, 3, 2 }; // [W, S, E, N]
    protected static final int[] x = { 0, 0, 1, 1 };
    protected static final int[] z = { 0, 1, 1, 0 };
    protected static final float eps = 1e-3f;

    @Nonnull protected final Optional<TRSRTransformation> transformation;
    @Nonnull protected final ImmutableMap<ItemCameraTransforms.TransformType, TRSRTransformation> transforms;
    protected final boolean hasTransform;

    @Nonnull protected final VertexFormat format;
    @Nonnull protected final TextureAtlasSprite still, flowing;
    @Nonnull protected final Optional<TextureAtlasSprite> overlay;

    protected final float[] argb = new float[4];
    protected final boolean gas;

    public BakedModelFluid(@Nonnull Fluid fluidIn, @Nonnull IModelState stateIn, @Nonnull VertexFormat formatIn, @Nonnull Function<ResourceLocation, TextureAtlasSprite> textureGetterIn) { this(fluidIn, fluidIn.getStill(), fluidIn.getFlowing(), stateIn, formatIn, textureGetterIn); }
    public BakedModelFluid(@Nonnull Fluid fluidIn, @Nonnull ResourceLocation stillIn, @Nonnull ResourceLocation flowingIn, @Nonnull IModelState stateIn, @Nonnull VertexFormat formatIn, @Nonnull Function<ResourceLocation, TextureAtlasSprite> textureGetterIn) {
        transformation = stateIn.apply(Optional.empty());
        transforms = PerspectiveMapWrapper.getTransforms(stateIn);
        hasTransform = transformation.isPresent() && !transformation.get().isIdentity();

        format = formatIn;
        still = textureGetterIn.apply(stillIn);
        flowing = textureGetterIn.apply(flowingIn);
        overlay = Optional.ofNullable(fluidIn.getOverlay()).map(textureGetterIn);

        final int color = fluidIn.getColor();
        argb[0] = ((color >> 24) & 0xFF) / 255f;
        argb[1] = ((color >> 16) & 0xFF) / 255f;
        argb[2] = ((color >> 8)  & 0xFF) / 255f;
        argb[3] = ( color        & 0xFF) / 255f;
        gas = fluidIn.isLighterThanAir();
    }

    @Nonnull
    @Override
    public List<BakedQuad> getQuads(@Nullable IBlockState state, @Nullable EnumFacing side, long rand) {
        if(side != null) {
            if(state instanceof PluginBlockFluidBase.FluidExtendedBlockState) {
                final PluginBlockFluidBase.FluidExtendedBlockState extended = (PluginBlockFluidBase.FluidExtendedBlockState)state;
                if(!extended.shouldSideBeRenderedCache[side.ordinal()]) return Collections.emptyList(); //if side cannot render, don't bother calculating quads

                final EnumFacing.Axis axis = side.getAxis();
                if(axis == EnumFacing.Axis.Y) {
                    // top
                    final EnumFacing top = gas ? EnumFacing.DOWN : EnumFacing.UP;
                    if(side == top) {
                        // y levels
                        final float[] y = new float[4];
                        boolean fullVolume = true;
                        for(int i = 0; i < 4; i++) {
                            final float value = extended.levelCorners[i];
                            if(value < 1) fullVolume = false;
                            y[i] = gas ? 1 - value : value;
                        }

                        // flow
                        final boolean isFlowing = extended.flowDirection != null && extended.flowDirection > -1000;
                        final float flow = isFlowing ? extended.flowDirection : 0;

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

                        final List<BakedQuad> quads = new LinkedList<>();
                        quads.add(buildQuad(top, topSprite, gas, false, topX, topY, topZ, topU, topV));
                        if(!fullVolume) quads.add(buildQuad(top, topSprite, !gas, true, topX, topY, topZ, topU, topV));

                        return Collections.unmodifiableList(quads);
                    }

                    // bottom
                    else return Collections.singletonList(buildQuad(top.getOpposite(), still, gas, false, i -> z[i], i -> gas ? 0.998f : 0.002f, i -> x[i], i -> z[i] << 4, i -> x[i] << 4));
                }

                // sides
                else {
                    final boolean useOverlay = overlay.isPresent() && extended.sideOverlays[side.getHorizontalIndex()];
                    final int si = WSEN[side.getHorizontalIndex()]; // local var for lambda capture

                    final float[] y = new float[4];
                    for(int i = 0; i < 4; i++) y[i] = gas ? 1 - extended.levelCorners[i] : extended.levelCorners[i];

                    final VertexParameter sideX = j -> fixTextureFightingX(x[(si + x[j]) & 3], side);
                    final VertexParameter sideY = j -> z[j] == 0 ? (gas ? 1 : 0) : y[(si + x[j]) & 3];
                    final VertexParameter sideZ = j -> fixTextureFightingZ(z[(si + x[j]) & 3], side);
                    final VertexParameter sideU = j -> x[j] << 3;
                    final VertexParameter sideV = j -> (gas ? sideY.get(j) : 1 - sideY.get(j)) * 8;

                    final List<BakedQuad> quads = new LinkedList<>();
                    if(!useOverlay) quads.add(buildQuad(side, flowing, gas, true, sideX, sideY, sideZ, sideU, sideV));
                    quads.add(buildQuad(side, useOverlay ? overlay.get() : flowing, !gas, false, sideX, sideY, sideZ, sideU, sideV));

                    return Collections.unmodifiableList(quads);
                }
            }

            // inventory
            else if(side == EnumFacing.SOUTH) return Collections.singletonList(buildQuad(EnumFacing.UP, still, false, false, i -> z[i], i -> x[i], i -> 0, i -> z[i] << 4, i -> x[i] << 4));
        }

        return Collections.emptyList();
    }

    @Nonnull
    protected BakedQuad buildQuad(@Nonnull EnumFacing side, @Nonnull TextureAtlasSprite texture, boolean flip, boolean offset, @Nonnull VertexParameter x, @Nonnull VertexParameter y, @Nonnull VertexParameter z, @Nonnull VertexParameter u, @Nonnull VertexParameter v) {
        final UnpackedBakedQuad.Builder builder = new UnpackedBakedQuad.Builder(format);
        final IVertexConsumer consumer = hasTransform ? new TRSRTransformer(builder, transformation.get()) : builder;

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
    public boolean isAmbientOcclusion() { return false; }

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

    public static float fixTextureFightingX(float old, @Nonnull EnumFacing side) {
        return side.getAxis() == EnumFacing.Axis.Z ? old : old == 1 ? 0.998f : 0.002f;
    }

    public static float fixTextureFightingZ(float old, @Nonnull EnumFacing side) {
        return side.getAxis() == EnumFacing.Axis.X ? old : old == 1 ? 0.998f : 0.002f;
    }

    // maps vertex index to parameter value
    @FunctionalInterface
    public interface VertexParameter { float get(int index); }
}
