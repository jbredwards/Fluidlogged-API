/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.datafix;

import net.minecraft.block.Block;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.NibbleArray;
import net.minecraftforge.common.util.Constants;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

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
    public static final List<Mapper> STATE_MAPPERS = new ArrayList<>();

    @Nonnull
    public static NBTTagCompound fix(@Nonnull final NBTTagCompound compound) {
        if(STATE_MAPPERS.isEmpty()) return compound;
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
                    @Nullable final ResourceLocation blockName = block.getRegistryName();
                    if(blockName != null) { // should never pass, but let's be safe
                        @Nonnull final Optional<Pair<OptionalInt, String>> mapping = STATE_MAPPERS.stream()
                                .map(mapper -> mapper.map(blockName, blockID, blockMeta))
                                .filter(Objects::nonNull).findFirst();
                        if(mapping.isPresent()) {
                            @Nonnull final NBTTagCompound nbt = new NBTTagCompound();
                            nbt.setString("id", mapping.get().getRight());
                            nbt.setLong("pos", posBuilder.setPos(blockX | x, blockY | y, blockZ | z).toLong());
                            /* Store a fluid location for later */
                            capabilityData.appendTag(nbt);
                            metadataArray.set(x, y, z, mapping.get().getLeft().orElse(blockMeta));
                        }
                    }
                }
            }
        }

        return compound;
    }

    public interface Mapper
    {
        /**
         * @param blockName The block registry name.
         * @param blockID The old block id. Useful for remapped blocks.
         * @param blockMetadata The block metadata.
         * @return A pair containing an optional new metadata value for the block, and the fluid block to store as a FluidState.
         * To remap a block, use Forge's {@link net.minecraftforge.event.RegistryEvent.MissingMappings MissingMappings} event.
         * If you want to map a block that was remapped: cache its
         * {@link net.minecraftforge.event.RegistryEvent.MissingMappings.Mapping#id old ID} and compare it with the blockID provided.
         */
        @Nullable
        Pair<OptionalInt, String> map(@Nonnull final ResourceLocation blockName, final int blockID, final int blockMetadata);
    }
}
