package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.material.Material;
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
        /*
         * blocksMovement:
         * New code:
         * //prevents fluids from destroying "circuit" blocks
         * @ASMGenerated
         * public blocksMovement()
         * {
         *     return Hooks.isMaterialCircuit(this);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_76230_c" : "blocksMovement"),
            "isMaterialCircuit", "(Lnet/minecraft/block/material/Material;)Z", generator -> generator.visitVarInsn(ALOAD, 0)
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isMaterialCircuit(@Nonnull Material material) { return material == Material.CIRCUITS; }
    }
}
