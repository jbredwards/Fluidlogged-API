package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.client.model.BakedModelFluid;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.renderer.vertex.VertexFormat;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.model.IModelState;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.function.Function;

/**
 * fixes all issues with fluidlogged z-fighting
 * @author jbred
 *
 */
public final class PluginModelFluid implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, "lambda$buildQuads$13", "(II)F")) return 1;
        else if(checkMethod(method, "lambda$buildQuads$11", "(II)F")) return 2;
        else return checkMethod(method, "lambda$buildQuads$7", "(I)F") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * buildQuads: (changes are around line 356)
         * Old code:
         * VertexParameter sideZ = j -> z[(si + x[j]) % 4];
         *
         * New code:
         * //offsets sides along the z-axis by 0.002 to fix fluidlogged z-fighting
         * VertexParameter sideZ = j -> Hooks.fixTextureFightingZ(z[(si + x[j]) % 4], si);
         */
        if(index == 1 && insn.getOpcode() == I2F) {
            final InsnList list = new InsnList();
            //si local var
            list.add(new VarInsnNode(ILOAD, 0));
            //method
            list.add(genMethodNode("fixTextureFightingZ", "(FI)F"));

            instructions.insert(insn, list);
            return true;
        }
        /*
         * buildQuads: (changes are around line 354)
         * Old code:
         * VertexParameter sideX = j -> x[(si + x[j]) % 4];
         *
         * New code:
         * //offsets sides along the x-axis by 0.002 to fix fluidlogged z-fighting
         * VertexParameter sideX = j -> Hooks.fixTextureFightingX(x[(si + x[j]) % 4], si);
         */
        else if(index == 2 && insn.getOpcode() == I2F) {
            final InsnList list = new InsnList();
            //si local var
            list.add(new VarInsnNode(ILOAD, 0));
            //method
            list.add(genMethodNode("fixTextureFightingX", "(FI)F"));

            instructions.insert(insn, list);
            return true;
        }
        /*
         * buildQuads: (changes are around line 355)
         * Old code:
         * VertexParameter sideY = j -> z[j] == 0 ? (gas ? 1 : 0) : y[(si + x[j]) % 4];
         *
         * New code:
         * //offsets the bottom by 0.002 to fix fluidlogged z-fighting
         * VertexParameter sideY = j -> z[j] == 0 ? (gas ? 0.998f : 0.002f) : y[(si + x[j]) % 4];
         */
        else if(index == 3) {
            //gas
            if(insn.getOpcode() == FCONST_1) {
                instructions.insert(insn, new LdcInsnNode(0.998f));
                instructions.remove(insn);
            }
            //normal
            if(insn.getOpcode() == FCONST_0) {
                instructions.insert(insn, new LdcInsnNode(0.002f));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        if("net/minecraftforge/client/model/ModelFluid".equals(classNode.name)) {
            /*
             * bake:
             * //direct to new baked fluid model instance that doesn't use a cache (fixes issue#176)
             * @Override
             * public IBakedModel bake(IModelState state, VertexFormat format, Function<ResourceLocation, TextureAtlasSprite> bakedTextureGetter)
             * {
             *     return Hooks.bake(this.fluid, this.fluid.getStill(), this.fluid.getFlowing(), state, format, bakedTextureGetter);
             * }
             */
            overrideMethod(classNode, method -> method.name.equals("bake"), "bake", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/util/ResourceLocation;Lnet/minecraft/util/ResourceLocation;Lnet/minecraftforge/common/model/IModelState;Lnet/minecraft/client/renderer/vertex/VertexFormat;Ljava/util/function/Function;)Lnet/minecraft/client/renderer/block/model/IBakedModel;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/client/model/ModelFluid", "fluid", "Lnet/minecraftforge/fluids/Fluid;");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/client/model/ModelFluid", "fluid", "Lnet/minecraftforge/fluids/Fluid;");
                generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/Fluid", "getStill", "()Lnet/minecraft/util/ResourceLocation;", false);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/client/model/ModelFluid", "fluid", "Lnet/minecraftforge/fluids/Fluid;");
                generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/Fluid", "getFlowing", "()Lnet/minecraft/util/ResourceLocation;", false);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            });

            return false;
        }

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        @SideOnly(Side.CLIENT)
        public static IBakedModel bake(@Nonnull Fluid fluid, @Nonnull ResourceLocation still, @Nonnull ResourceLocation flowing, @Nonnull IModelState state, @Nonnull VertexFormat format, @Nonnull Function<ResourceLocation, TextureAtlasSprite> textureGetter) {
            return new BakedModelFluid(fluid, still, flowing, state, format, textureGetter);
        }

        //[W, S, E, N]
        @SideOnly(Side.CLIENT)
        public static float fixTextureFightingX(float old, int index) {
            return BakedModelFluid.fixTextureFightingX(old, EnumFacing.byHorizontalIndex(5 - index));
        }

        //[W, S, E, N]
        @SideOnly(Side.CLIENT)
        public static float fixTextureFightingZ(float old, int index) {
            return BakedModelFluid.fixTextureFightingZ(old, EnumFacing.byHorizontalIndex(5 - index));
        }
    }
}
