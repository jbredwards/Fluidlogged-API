package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * fix MC-80966
 * @author jbred
 *
 */
public final class PluginExtendedBlockStorage implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * New code:
         * //fix MC-80966
         * public boolean isEmpty()
         * {
         *     return false;
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_76663_a" : "isEmpty"), null, null, generator -> generator.visitInsn(ICONST_0));
        return false;
    }
}
