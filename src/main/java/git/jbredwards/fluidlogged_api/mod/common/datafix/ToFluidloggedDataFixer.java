/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.datafix;

import git.jbredwards.fluidlogged_api.api.datafix.IFluidloggedDataMapper;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.NibbleArray;
import net.minecraftforge.common.util.Constants;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;
import java.util.OptionalInt;

/**
 * Converts certain fake "pseudo-fluidlogged" blocks into real ones.
 * <p>
 * Useful for mods that want to migrate to a Fluidlogged API dependency.
 * </p>
 * @author jbred
 *
 */
public final class ToFluidloggedDataFixer
{
    @Nonnull
    public static NBTTagCompound fix(@Nonnull final NBTTagCompound compound) {
        if(IFluidloggedDataMapper.MAPPERS.isEmpty()) return compound;
        else {
            @Nonnull final BlockPos.MutableBlockPos posBuilder = new BlockPos.MutableBlockPos();

            @Nonnull final NBTTagCompound level = compound.getCompoundTag("Level");
            @Nonnull final NBTTagList sections = level.getTagList("Sections", Constants.NBT.TAG_COMPOUND);
            @Nonnull final NBTTagList capabilityData = FluidloggedAPIFixableData.getOrCreateFluidCapabilityData(level);

            final int blockX = level.getInteger("xPos") << 4;
            final int blockZ = level.getInteger("zPos") << 4;
            for(int i = 0; i < sections.tagCount(); i++) {
                @Nonnull final NBTTagCompound section = sections.getCompoundTagAt(i);

                @Nonnull final byte[] blockIDs = section.getByteArray("Blocks");
                /* Use a blank extended ID array if not present */
                @Nonnull final NibbleArray extIDs = section.hasKey("Add", Constants.NBT.TAG_BYTE_ARRAY) ? new NibbleArray(section.getByteArray("Add")) : new NibbleArray();
                @Nonnull final NibbleArray metadataArray = new NibbleArray(section.getByteArray("Data"));

                final int blockY = section.getInteger("Y") << 4;
                for(int pos = 0; pos < blockIDs.length; pos++) {
                    final int x = pos & 15;
                    final int y = pos >> 8 & 15;
                    final int z = pos >> 4 & 15;

                    /* Find the real block ID by combining the extended ID and the normal ID */
                    final int blockID = extIDs.get(x, y, z) << 8 | (blockIDs[pos] & 255);
                    final int blockMeta = metadataArray.get(x, y, z);

                    @Nonnull final Block block = Block.getBlockById(blockID);
                    if(block.getRegistryName() != null) { // ensure the block is registered
                        @Nullable final Pair<OptionalInt, FluidState> mapping = IFluidloggedDataMapper.MAPPERS.get(block).stream()
                                .map(mapper -> mapper.remapFluidData(blockID, blockMeta))
                                .filter(Objects::nonNull).findFirst().orElse(null);

                        if(mapping != null) {
                            mapping.getLeft().ifPresent(meta -> metadataArray.set(x, y, z, meta));
                            if(!mapping.getRight().isEmpty()) {
                                @Nonnull final NBTTagCompound nbt = new NBTTagCompound();
                                nbt.setLong("pos", posBuilder.setPos(blockX | x, blockY | y, blockZ | z).toLong());
                                nbt.setString("id", String.valueOf(mapping.getRight().getBlock().getRegistryName()));
                                nbt.setInteger("meta", mapping.getRight().getMetadata());
                                /* Store a fluid data for later */
                                capabilityData.appendTag(nbt);
                            }
                        }
                    }
                }
            }
        }

        return compound;
    }
}
