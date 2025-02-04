/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.capability;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;

import javax.annotation.Nonnull;

/**
 * Used to serialize position data, to lighten the load put into chunks or packets when sending data to clients.
 * This serializer has local x, y, and z positions, which are used during the deserialization process.
 *
 * @since 3.0.0
 * @author jbred
 *
 */
public interface IPosSerializer
{
    /**
     * @param x The x position to serialize.
     * @return Serialized x position (with the proper bit shifts applied).
     *
     * @since 3.0.0
     * @author jbred
     */
    int serializeX(final int x);

    /**
     * @param y The y position to serialize.
     * @return Serialized y position (with the proper bit shifts applied).
     *
     * @since 3.0.0
     * @author jbred
     */
    int serializeY(final int y);

    /**
     * @param z The z position to serialize.
     * @return Serialized z position (with the proper bit shifts applied).
     *
     * @since 3.0.0
     * @author jbred
     */
    int serializeZ(final int z);

    /**
     * @param x The x position to serialize.
     * @param y The y position to serialize.
     * @param z The z position to serialize.
     * @return Serialized position.
     *
     * @since 3.0.0
     * @author jbred
     */
    default char serializePos(final int x, final int y, final int z) {
        return (char)(serializeX(x) | serializeY(y) | serializeZ(z));
    }

    /**
     * @param pos The position to serialize.
     * @return Serialized position.
     * @throws NullPointerException If pos is null.
     *
     * @since 3.0.0
     * @author jbred
     */
    default char serializePos(@Nonnull final Vec3i pos) {
        return serializePos(pos.getX(), pos.getY(), pos.getZ());
    }

    /**
     * @param serializedPos Either the serialized position or the serialized x position.
     * @return Deserialized x position.
     *
     * @since 3.0.0
     * @author jbred
     */
    int deserializeX(final char serializedPos);

    /**
     * @param serializedPos Either the serialized position or the serialized y position.
     * @return Deserialized y position.
     *
     * @since 3.0.0
     * @author jbred
     */
    int deserializeY(final char serializedPos);

    /**
     * @param serializedPos Either the serialized position or the serialized z position.
     * @return Deserialized z position.
     *
     * @since 3.0.0
     * @author jbred
     */
    int deserializeZ(final char serializedPos);

    /**
     * @param serializedPos The serialized position.
     * @return Deserialized position.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    default BlockPos deserializePos(final char serializedPos) {
        return new BlockPos(deserializeX(serializedPos), deserializeY(serializedPos), deserializeZ(serializedPos));
    }
}
