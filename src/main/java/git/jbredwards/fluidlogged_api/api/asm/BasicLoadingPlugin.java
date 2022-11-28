package git.jbredwards.fluidlogged_api.api.asm;

import net.minecraftforge.fml.relauncher.IFMLLoadingPlugin;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;

/**
 * Don't rewrite the same code for each IFMLLoadingPlugin
 * @author jbred
 *
 */
public interface BasicLoadingPlugin extends IFMLLoadingPlugin
{
    @Nonnull
    default String getPluginClass() { return getClass().getName() + "$Transformer"; }

    @Nonnull
    @Override
    default String[] getASMTransformerClass() { return new String[] {getPluginClass()}; }

    @Override
    default void injectData(@Nonnull Map<String, Object> data) {}

    @Nullable
    @Override
    default String getModContainerClass() { return null; }

    @Nullable
    @Override
    default String getSetupClass() { return null; }

    @Nullable
    @Override
    default String getAccessTransformerClass() { return null; }
}
