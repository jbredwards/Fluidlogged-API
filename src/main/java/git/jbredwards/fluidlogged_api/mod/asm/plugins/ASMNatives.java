package git.jbredwards.fluidlogged_api.mod.asm.plugins;

import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import net.minecraft.block.Block;
import net.minecraft.block.BlockBush;
import net.minecraft.block.state.IBlockState;
import net.minecraft.world.gen.structure.template.Template;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * methods for this class are built during runtime via PluginASMNatives
 * @author jbred
 *
 */
public final class ASMNatives
{
    /**
     * {@link Block}
     */
    @Nullable
    public static native ConfigHandler.ICanFluidFlowHandler getCanFluidFlow(@Nonnull Block block);
    public static native void setCanFluidFlow(@Nonnull Block block, @Nullable ConfigHandler.ICanFluidFlowHandler canFluidFlow);

    /**
     * {@link BlockBush}
     */
    public static native boolean canSustainBush(@Nonnull BlockBush bush, @Nonnull IBlockState state);

    /**
     * {@link Template}
     */
    public static native void setKeepOldFluidStates(@Nonnull Template template, boolean keepOldFluidStates);
}
