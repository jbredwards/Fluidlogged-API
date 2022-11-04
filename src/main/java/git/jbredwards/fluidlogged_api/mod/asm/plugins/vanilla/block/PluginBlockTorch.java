package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * allows torches to be destroyed by flowing fluid blocks
 * @author jbred
 *
 */
public final class PluginBlockTorch implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        addMethod(classNode, obfuscated ? "func_149688_o" : "getMaterial", "(Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/material/Material;",
            "getTorchMaterial", "(Lnet/minecraft/block/material/Material;Lnet/minecraft/block/Block;)Lnet/minecraft/block/material/Material;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
                generator.visitVarInsn(ALOAD, 0);
            }
        );

        return false;
    }
}
