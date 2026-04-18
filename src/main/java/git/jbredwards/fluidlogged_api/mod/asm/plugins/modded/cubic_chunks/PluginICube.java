/*
 * Copyright (C) <2026 to Present> <jbredwards>
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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cubic_chunks;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.world.ICubeData;
import io.github.opencubicchunks.cubicchunks.api.world.ICube;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * make Cubic Chunks ICube implement ICubeData
 * @author jbred
 *
 */
public final class PluginICube implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.interfaces.add(getAccessorClass());
        return false;
    }

    @SuppressWarnings("unused")
    public interface Accessor extends ICapabilityProvider, ICubeData
    {
        @Nonnull
        @Override
        default Chunk asChunk() {
            return ((ICube)this).getColumn();
        }

        @Nonnull
        @Override
        default FluidState getFluidState(final int x, final int y, final int z) {
            return FluidState.getFromProvider(this, x, y, z);
        }

        @Nullable
        @Override
        default TileEntity getTileEntity(final int x, final int y, final int z) {
            return getTileEntity(new BlockPos(x, y, z));
        }

        @Nullable
        @Override
        default TileEntity getTileEntity(@Nonnull final Vec3i pos) {
            return ((ICube)this).getTileEntity(pos instanceof BlockPos ? (BlockPos)pos : new BlockPos(pos), Chunk.EnumCreateEntityType.IMMEDIATE);
        }
    }
}
