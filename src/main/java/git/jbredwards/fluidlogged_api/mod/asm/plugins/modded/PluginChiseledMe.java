package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * fix chiseled me conflict, fluidlogged api already implements these patches
 * @author jbred
 *
 */
public final class PluginChiseledMe implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.methods.removeIf(method -> method.name.equals("rayTraceBlocks") || method.name.equals("isInLava"));
        return false;
    }
}
