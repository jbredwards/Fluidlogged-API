package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world.PluginWorldServer;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * spongeforge no longer mixins into conflicting methods
 * @author jbred
 *
 */
public final class PluginSpongeForge implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.methods.removeIf(method
                -> method.name.equals("spongeImpl$onEntityCollideWithBlockState")
                || method.name.equals("impl$CheckForLiquidMixing")
                || method.name.equals("impl$throwModifyForLavaToStone")
                || method.name.equals("impl$CheckEventsBeforeSpreadingFire")
                || method.name.equals("asyncLighting$onRelightChecksGetBlockState")
                || method.name.equals("onCheckDisplaceIfPreAlreadyCancelled"));

        return false;
    }
}
