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

package git.jbredwards.fluidlogged_api.api.world;

import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 * Gives {@link IBlockAccess} (non-{@link World}) instances the option to provide their {@link World} instance in a way that this mod can access it.
 * <p>
 * <b>{@link net.minecraft.world.World World} and {@link net.minecraft.world.ChunkCache ChunkCache} implement this at runtime, along with many modded classes.</b>
 * </p>
 * @since 3.0.0
 * @author jbred
 *
 */
public interface IWorldProvider
{
    /**
     * @return This {@link IBlockAccess}'s {@link World} instance. For any {@link World} instance, this returns itself.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    World getWorld();

    /**
     * @param access IBlockAccess.
     * @return The provided {@link IBlockAccess}'s {@link World} instance, or the client world as a fallback. For any {@link World}
     * instance, the built-in implementation of {@link IWorldProvider#getWorld()} simply returns itself. If null is provided, this returns null.
     * @throws IllegalArgumentException If access is not a {@link IWorldProvider} and this is not being executed from a client-side thread.
     * @throws NullPointerException If there is no client world loaded and this is being executed from a client-side thread.
     *
     * @since 3.0.0
     * @author jbred
     */
    static World getWorld(@Nullable final IBlockAccess access) {
        if(access instanceof IWorldProvider) return ((IWorldProvider)access).getWorld();
        else if(access == null) return null; // let's allow null to be used, and return null back
        else if(!FMLLaunchHandler.isDeobfuscatedEnvironment() && // easier issue detection for devs
                FMLCommonHandler.instance().getSide().isClient() && FMLCommonHandler.instance().getEffectiveSide().isClient())

            return Objects.requireNonNull(getWorldClient(), "Cannot get client world while none is loaded.");
        else throw new IllegalArgumentException("Could not get world from: \"" + access.getClass().toGenericString() + '"');
    }

    /**
     * @return The client world as a {@link World}, to fix a possible server-side crash.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nullable
    @SideOnly(Side.CLIENT)
    static World getWorldClient() { return FMLClientHandler.instance().getWorldClient(); }
}
