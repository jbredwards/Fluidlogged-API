/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.asm;

import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;

/**
 * An easy way of implementing {@link IASMPlugin IASMPlugin-based} transformers at runtime.
 *
 * @since 1.9.0
 * @author jbred
 *
 */
public abstract class AbstractClassTransformer implements IClassTransformer
{
    /**
     * Used by {@link AbstractClassTransformer#transform} to find which {@link IASMPlugin} to apply.
     * This map should be filled in your transformer's constructor.
     *
     * @since 1.9.0
     */
    @Nonnull
    protected final Map<String, IASMPlugin> plugins = new HashMap<>();

    /**
     * Inherited from {@link IClassTransformer}. This implementation applies transformations based on which
     * of your {@link AbstractClassTransformer#plugins} maps to the provided transformedName.
     *
     * @param name Name of the class to be transformed. This may be obfuscated.
     * @param transformedName Deobfuscated name of the class to be transformed.
     * @param basicClass Bytecode of the class to be transformed.
     * @return The bytecode to be used during runtime.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nullable
    @Override
    public byte[] transform(@Nullable final String name, @Nullable final String transformedName, @Nullable final byte[] basicClass) {
        if(basicClass == null || transformedName == null) return basicClass;
        @Nullable final IASMPlugin plugin = plugins.get(transformedName);
        if(plugin == null) return basicClass;

        IASMPlugin.setActivePlugin(getPluginName());
        @Nonnull final byte[] newClass = plugin.transform(basicClass, !FMLLaunchHandler.isDeobfuscatedEnvironment());
        IASMPlugin.resetActivePlugin();

        return newClass;
    }

    /**
     * @return The {@link net.minecraftforge.fml.relauncher.IFMLLoadingPlugin.Name name} of the
     * {@link net.minecraftforge.fml.relauncher.IFMLLoadingPlugin IFMLLoadingPlugin} that owns
     * this transformer. This method is used to help print debug info to the console as your transformer's
     * {@link AbstractClassTransformer#plugins} are applied.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    public abstract String getPluginName();
}
