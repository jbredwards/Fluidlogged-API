/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dynamictrees;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * remove water model, as the water is now handled via fluidlogging
 * @author jbred
 *
 */
public final class PluginModelRootyWater implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // Remove water model, as the water is now handled via fluidlogging
         * @ASMOverwrite
         * public List<BakedQuad> getQuads(IBlockState state, EnumFacing side, long rand)
         * {
         *     return this.rootsModel.getQuads(state, side, rand);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_188616_a" : "getQuads"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "com/ferreusveritas/dynamictrees/models/ModelRootyWater", "rootsModel", "Lnet/minecraft/client/renderer/block/model/IBakedModel;");
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(LLOAD, 3);
            generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraft/client/renderer/block/model/IBakedModel", obfuscated ? "func_188616_a" : "getQuads", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;J)Ljava/util/List;", true);
        });
        /*
         * New code:
         * // Use root's particle texture instead of water
         * @ASMOverwrite
         * public TextureAtlasSprite getParticleTexture()
         * {
         *     return this.rootsModel.getParticleTexture();
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_177554_e" : "getParticleTexture"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "com/ferreusveritas/dynamictrees/models/ModelRootyWater", "rootsModel", "Lnet/minecraft/client/renderer/block/model/IBakedModel;");
            generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraft/client/renderer/block/model/IBakedModel", obfuscated ? "func_177554_e" : "getParticleTexture", "()Lnet/minecraft/client/renderer/texture/TextureAtlasSprite;", true);
        });

        return false;
    }
}
