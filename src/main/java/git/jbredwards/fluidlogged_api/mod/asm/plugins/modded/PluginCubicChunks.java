package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * Move mixins to asm to prevent crash, gosh why is mixin so bad and why do so many people learn it over asm? geez
 * For real though, this mod doesn't even change the code the mixin is looking for... and yet this is still necessary
 * @author jbred
 *
 */
public final class PluginCubicChunks implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.methods.removeIf(method -> method.name.equals("getLightForExt_getMinHeight")
                || method.name.equals("getLightForExt_getMaxHeight"));

        return false;
    }
}
