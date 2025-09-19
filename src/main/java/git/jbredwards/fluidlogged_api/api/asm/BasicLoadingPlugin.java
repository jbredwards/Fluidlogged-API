/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.api.asm;

import net.minecraftforge.fml.relauncher.IFMLLoadingPlugin;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;

/**
 * A bare-bones implementation of {@link IFMLLoadingPlugin}. This also automatically looks for a nested transformer class
 * to use as its class transformer. See Fluidlogged API's {@link git.jbredwards.fluidlogged_api.mod.asm.ASMHandler ASMHandler}
 * for an example.
 * <p>
 * <strong>
 * Note: At this time (7/14/2024), the
 * <a href="https://github.com/TerraFirmaCraft-The-Final_Frontier/FileDirector">
 * File Director
 * </a>
 * mod can throw a {@link ClassNotFoundException} if any {@link IFMLLoadingPlugin} implements anything from an
 * external (non-forge) library. Until that bug is resolved, this interface should only be used by Fluidlogged API.
 * </strong>
 * </p>
 *
 * @since 1.9.0
 * @author jbred
 *
 */
public interface BasicLoadingPlugin extends IFMLLoadingPlugin
{
    /**
     * @return A class that implements the {@link net.minecraft.launchwrapper.IClassTransformer IClassTransformer} interface.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    default String getPluginClass() { return getClass().getName() + "$Transformer"; }

    /**
     * @return A list of classes that implement the {@link net.minecraft.launchwrapper.IClassTransformer IClassTransformer} interface.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nonnull
    @Override
    default String[] getASMTransformerClass() { return new String[] {getPluginClass()}; }

    /**
     * Inject coremod data into this coremod. This data includes:
     * <ul>
     * <li>"mcLocation" - Location of the minecraft directory.</li>
     * <li>"coremodList" - The list of coremods.</li>
     * <li>"coremodLocation" - File this coremod loaded from.</li>
     * <li>"runtimeDeobfuscationEnabled" - True if being ran from a deobfuscated environment.</li>
     * </ul>
     *
     * @since 1.9.0
     * @author jbred
     */
    @Override
    default void injectData(@Nonnull final Map<String, Object> data) {}

    /**
     * @return A class name that implements {@link net.minecraftforge.fml.common.ModContainer ModContainer} for
     * injection into the mod list. This mod container will be loaded before all regular mod containers, which means it
     * will be forced to be "immutable" - not susceptible   sorting behaviour. All other mod behaviours are available
     * however- this container can receive and handle normal loading events.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nullable
    @Override
    default String getModContainerClass() { return null; }

    /**
     * @return The class name of an implementor of {@link net.minecraftforge.fml.relauncher.IFMLCallHook IFMLCallHook},
     * that will be run, in the main thread, to perform any additional setup this coremod may require. It will be
     * run <strong>prior</strong> to Minecraft starting, so it CANNOT operate on minecraft itself.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nullable
    @Override
    default String getSetupClass() { return null; }

    /**
     * @return The name of an access transformer class. It will be injected post-deobf to ensure your ATs conform to
     * the new srgnames scheme.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nullable
    @Override
    default String getAccessTransformerClass() { return null; }
}
