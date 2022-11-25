package git.jbredwards.fluidlogged_api.api.asm.impl;

import net.minecraft.client.Minecraft;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Gives server-side IBlockAccess (non-World) instances the option to provide chunks in a way that this mod can access them.
 * The {@link net.minecraft.world.World World} and {@link net.minecraft.world.ChunkCache ChunkCache} classes implement this at runtime.
 * @author jbred
 *
 */
public interface IChunkProvider
{
    @Nullable
    Chunk getChunkFromBlockCoords(@Nonnull BlockPos pos);

    @Nullable
    static Chunk getChunk(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return world instanceof IChunkProvider ? ((IChunkProvider)world).getChunkFromBlockCoords(pos) : null;
    }

    @Nullable
    @SideOnly(Side.CLIENT)
    static Chunk getClientChunk(@Nonnull BlockPos pos) { return getChunk(Minecraft.getMinecraft().world, pos); }
}

