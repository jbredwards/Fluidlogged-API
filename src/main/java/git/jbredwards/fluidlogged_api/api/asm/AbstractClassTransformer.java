package git.jbredwards.fluidlogged_api.api.asm;

import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;

import javax.annotation.Nonnull;
import java.util.HashMap;
import java.util.Map;

/**
 * An easy way of implementing IASMPlugin-based transformers at runtime
 * @author jbred
 *
 */
public abstract class AbstractClassTransformer implements IClassTransformer
{
    @Nonnull
    protected final Map<String, IASMPlugin> plugins = new HashMap<>();

    @Nonnull
    @Override
    public byte[] transform(@Nonnull String name, @Nonnull String transformedName, @Nonnull byte[] basicClass) {
        final IASMPlugin plugin = plugins.get(transformedName);
        if(plugin == null) return basicClass;

        IASMPlugin.ACTIVE_TRANSFORMER.value = getPluginName();
        return plugin.transform(basicClass, !FMLLaunchHandler.isDeobfuscatedEnvironment());
    }

    @Nonnull
    public abstract String getPluginName();
}
