package git.jbredwards.fluidlogged_api.asm;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import git.jbredwards.fluidlogged_api.asm.plugins.forge.*;
import git.jbredwards.fluidlogged_api.asm.plugins.vanilla.*;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.IFMLLoadingPlugin;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;

/**
 * handler for only this mod's plugins
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@IFMLLoadingPlugin.SortingIndex(1401)
@IFMLLoadingPlugin.Name("Fluidlogged API Plugin")
@IFMLLoadingPlugin.MCVersion("1.12.2")
public final class ASMHandler implements IFMLLoadingPlugin
{
    //this class exists cause the vanilla launcher needs the transformer & plugin to be different classes for reasons?
    public static final class Transformer implements IClassTransformer
    {
        //plugin registry
        @Nonnull
        public static Map<String, IASMPlugin> PLUGINS = new ImmutableMap.Builder<String, IASMPlugin>()
                //vanilla
                .put("net.minecraft.client.renderer.EntityRenderer", new EntityRendererPlugin())
                .put("net.minecraft.world.World", new WorldPlugin())
                .put("net.minecraft.world.WorldServer", new WorldServerPlugin())
                //forge
                .put("net.minecraftforge.common.ForgeHooks", new ForgeHooksPlugin())
                .put("net.minecraftforge.fluids.Fluid", new FluidPlugin())
                .build();

        @Override
        public byte[] transform(String name, String transformedName, byte[] basicClass) {
            final @Nullable IASMPlugin plugin = PLUGINS.get(transformedName);
            return plugin == null ? basicClass : plugin.transform(basicClass, !name.equals(transformedName));
        }
    }

    @Override
    public String[] getASMTransformerClass() {
        return new String[] {
                "git.jbredwards.fluidlogged_api.asm.ASMReplacer",
                "git.jbredwards.fluidlogged_api.asm.ASMHandler$Transformer"
        };
    }

    @Nullable
    @Override
    public String getModContainerClass() { return null; }

    @Nullable
    @Override
    public String getSetupClass() { return null; }

    @Override
    public void injectData(@Nullable Map<String, Object> data) { }

    @Nullable
    @Override
    public String getAccessTransformerClass() { return null; }
}
