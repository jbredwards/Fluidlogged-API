package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * duplicate fluid logic isn't needed, and causes conflicts with this mod
 * @author jbred
 *
 */
public final class PluginTFCBlockFluid implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.methods.removeIf(method ->
                method.name.equals(obfuscated ? "func_180650_b" : "updateTick") ||
                method.name.equals("getOptimalFlowDirections") ||
                method.name.equals("calculateFlowCost") ||
                method.name.equals("flowIntoBlock") ||
                method.name.equals("canFlowInto") ||
                method.name.equals("canDisplace") ||
                method.name.equals("getExtendedState") ||
                method.name.equals("getFluidHeightForRender"));

        return false;
    }
}
