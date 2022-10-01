package git.jbredwards.fluidlogged_api.mod.asm.plugins;

import net.minecraft.world.gen.structure.template.Template;

import javax.annotation.Nonnull;

/**
 * methods for this class are built during runtime via PluginASMNatives
 * @author jbred
 *
 */
public final class ASMNatives
{
    /**
     * {@link Template}
     */
    public static native void setKeepOldFluidStates(@Nonnull Template template, boolean keepOldFluidStates);
}
