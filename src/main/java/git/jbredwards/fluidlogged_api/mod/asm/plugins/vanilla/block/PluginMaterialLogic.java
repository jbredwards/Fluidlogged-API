package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * prevents fluids from destroying "circuit" blocks
 * @author jbred
 *
 */
public final class PluginMaterialLogic implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_76230_c" : "blocksMovement"),
            "isMaterialCircuit", "(Lnet/minecraft/block/material/Material;)Z", generator -> {
                 generator.visitVarInsn(ALOAD, 0);
                 generator.visitMaxs(2, 0);
            }
        );

        return false;
    }
}
