/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.world;

import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.relauncher.FMLLaunchHandler;

import javax.annotation.Nonnull;

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
     * instance, the implementation of {@link IWorldProvider#getWorld() IWorldProvider.getWorld()} simply returns itself.
     * @throws IllegalArgumentException If access is not a {@link IWorldProvider} and this is not being executed from a client-side thread.
     * @throws NullPointerException If access is null and this is not being executed from a client-side thread.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    static World getWorld(@Nonnull final IBlockAccess access) {
        if(access instanceof IWorldProvider) return ((IWorldProvider)access).getWorld();
        else if(!FMLLaunchHandler.isDeobfuscatedEnvironment() && // easier issue detection for devs
                FMLCommonHandler.instance().getSide().isClient() && FMLCommonHandler.instance().getEffectiveSide().isClient())

            return FMLClientHandler.instance().getWorldClient();
        else throw new IllegalArgumentException("Could not get world from: \"" + access.getClass().toGenericString() + '"');
    }
}
