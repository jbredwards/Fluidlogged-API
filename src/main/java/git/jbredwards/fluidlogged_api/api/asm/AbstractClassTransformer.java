package git.jbredwards.fluidlogged_api.api.asm;

import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
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

    @Nullable
    @Override
    public byte[] transform(@Nonnull String name, @Nonnull String transformedName, @Nullable byte[] basicClass) {
        if(basicClass == null) return null;
        final IASMPlugin plugin = plugins.get(transformedName);
        if(plugin == null) return basicClass;

        IASMPlugin.setActivePlugin(getPluginName());
        basicClass = plugin.transform(basicClass, !FMLLaunchHandler.isDeobfuscatedEnvironment());
        IASMPlugin.resetActivePlugin();

        return basicClass;
    }

    @Nonnull
    public abstract String getPluginName();
}
