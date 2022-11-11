package git.jbredwards.fluidlogged_api.mod.common.legacy;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.datafix.IFixableData;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.NibbleArray;
import net.minecraftforge.common.util.Constants.NBT;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/* Inspired by https://github.com/Choonster-Minecraft-Mods/TestMod3/blob/5872bd0f917516ce1820d8749bfb75fcc83b0a60/src/main/java/choonster/testmod3/datafix/BlockFlattening.java */
/**
 * A Minecraft data fixer that remaps fluidlogged_api:[fluid]logged_te blocks to use the new save format
 * introduced in v1.7.
 * <dl><dt>This is accomplished through replacing the fluidlogged_te blocks with their stored blocks by reading the NBT data, and sets the original fluid to an IFluidStateCapability.</dt>
 */
@Mod.EventBusSubscriber(modid = FluidloggedAPI.MODID)
public final class LegacyDataFixer implements IFixableData {
    /**
     * A map which stores the block ID of the fluidlogged tile entity mapped to the corresponding fluid name.
     *
     * This is reset whenever the integrated server restarts, to make sure that IDs don't cross world boundaries.
     */
    private static Int2ObjectMap<String> FLUID_MAPPINGS = null;

    /**
     * A mapping between the fluid name used by the tile entity and a Forge fluid block.
     *
     * This is reset whenever the integrated server restarts, to make sure that IDs don't cross world boundaries.
     */
    private static Int2ObjectMap<Block> FLUID_ID_MAPPINGS = null;

    /**
     * The current data version. This is an arbritary value since the mod never used data fixers before.
     */
    public static final int DATA_VERSION = 101;

    /**
     * Called when FMLServerAboutToStartEvent is invoked.
     */
    public static void init() {
        FLUID_MAPPINGS = new Int2ObjectOpenHashMap<>();
        FLUID_ID_MAPPINGS = new Int2ObjectOpenHashMap<>();
    }

    /**
     * Called when FMLServerStoppedEvent is invoked.
     */
    public static void reset() {
        FLUID_MAPPINGS = null;
        FLUID_ID_MAPPINGS = null;
    }

    /**
     * This event handler is called when the registries are first loaded from level.dat. Any fluidlogged_te blocks
     * have their ID and fluid name stored in the {@link #FLUID_MAPPINGS} map.
     */
    @SubscribeEvent
    public static void onMissingBlockMappings(@Nonnull RegistryEvent.MissingMappings<Block> event) {
        for(RegistryEvent.MissingMappings.Mapping<Block> mapping : event.getAllMappings()) {
            if(mapping.key.getPath().endsWith("logged_te")) {
                FLUID_MAPPINGS.put(mapping.id, mapping.key.getPath().replace("logged_te", ""));
                /* Prevent a warning */
                mapping.ignore();
            }
        }
    }

    @Override
    public int getFixVersion() { return DATA_VERSION; }

    /**
     * Try to find a Forge fluid given an old blockID.
     */
    @Nullable
    private static Block getFluidByOldName(int blockID) {
        return FLUID_ID_MAPPINGS.computeIfAbsent(blockID, block -> {
            final @Nullable String mapping = FLUID_MAPPINGS.get(blockID);
            if(mapping != null) {
                final @Nullable Fluid fluid = FluidRegistry.getFluid(mapping);
                if(fluid != null) return fluid.getBlock();
                else System.err.println("Can't find fluid: " + mapping);
            }

            else System.err.println("Can't find fluid from id: " + blockID);
            return null;
        });
    }

    /**
     * Rewrites fluidlogged_te blocks to have the block ID and metadata of their contents.
     */
    @SuppressWarnings("deprecation")
    @Nonnull
    @Override
    public NBTTagCompound fixTagCompound(@Nonnull NBTTagCompound compound) {
        // Skip datafix if it isn't needed
        if(!FLUID_MAPPINGS.isEmpty()) {
            final NBTTagCompound level = compound.getCompoundTag("Level");
            final NBTTagList tileEntities = level.getTagList("TileEntities", NBT.TAG_COMPOUND);
            //store the pos & nbt of the tiles that need fixing
            final Int2ObjectMap<List<Pair<BlockPos, NBTTagCompound>>> legacyTiles = new Int2ObjectOpenHashMap<>();
            for(Iterator<NBTBase> it = tileEntities.iterator(); it.hasNext();) {
                //check if the te is from a legacy version, if it is: add it to fix list
                final NBTTagCompound nbt = (NBTTagCompound)it.next();
                if(nbt.getString("id").equals(FluidloggedAPI.MODID + ":te")) {
                    final BlockPos pos = new BlockPos(nbt.getInteger("x"), nbt.getInteger("y"), nbt.getInteger("z"));
                    final int sectionY = pos.getY() >> 4;
                    //get fluidlogged te's from the section
                    @Nullable List<Pair<BlockPos, NBTTagCompound>> list = legacyTiles.get(sectionY);
                    if(list == null) {
                        list = new ArrayList<>();
                        legacyTiles.put(sectionY, list);
                    }
                    //assigns fluidlogged te's to a section
                    list.add(Pair.of(pos, nbt));
                    //remove legacy tile entities from level data
                    it.remove();
                }
            }
            //fix legacy fluidlogged blocks
            if(!legacyTiles.isEmpty()) {
                //initialise or get forge capability nbt
                final NBTTagCompound forgeCaps;
                if(level.hasKey("ForgeCaps", NBT.TAG_COMPOUND))
                    forgeCaps = level.getCompoundTag("ForgeCaps");
                else {
                    forgeCaps = new NBTTagCompound();
                    level.setTag("ForgeCaps", forgeCaps);
                }
                //initialise or get IFluidStateCapability nbt
                final NBTTagList cap;
                if(forgeCaps.hasKey(IFluidStateCapability.CAPABILITY_ID.toString(), NBT.TAG_LIST))
                    cap = forgeCaps.getTagList(IFluidStateCapability.CAPABILITY_ID.toString(), NBT.TAG_COMPOUND);
                else {
                    cap = new NBTTagList();
                    forgeCaps.setTag(IFluidStateCapability.CAPABILITY_ID.toString(), cap);
                }
                final NBTTagList sections = level.getTagList("Sections", NBT.TAG_COMPOUND);
                for(int i = 0; i < sections.tagCount(); ++i) {
                    final NBTTagCompound section = sections.getCompoundTagAt(i);
                    final int sectionY = section.getByte("Y");
                    //find the fluidlogged te's within this section to fix
                    final @Nullable List<Pair<BlockPos, NBTTagCompound>> sectionLegacyTiles = legacyTiles.get(sectionY);
                    if(sectionLegacyTiles != null) {
                        final NibbleArray metadataArray = new NibbleArray(section.getByteArray("Data"));
                        final byte[] blockIDs = section.getByteArray("Blocks");
                        /* Use a blank extended ID array if not present */
                        final NibbleArray extIDs = section.hasKey("Add", NBT.TAG_BYTE_ARRAY) ?
                                new NibbleArray(section.getByteArray("Add")) : new NibbleArray();
                        //fix each fluidlogged te within the section
                        for(final Pair<BlockPos, NBTTagCompound> entry : sectionLegacyTiles) {
                            final BlockPos pos = entry.getKey();
                            //calculate section indices
                            final int x = pos.getX() & 15;
                            final int y = pos.getY() & 15;
                            final int z = pos.getZ() & 15;
                            final int blockIndex = y << 8 | z << 4 | x;
                            /* Find the real block ID by combining the extended ID and the normal ID */
                            final int blockID = extIDs.get(x, y, z) << 8 | (blockIDs[blockIndex] & 255);
                            /* Look up the originally stored block and meta */
                            final NBTTagCompound storedNbt = entry.getValue().getCompoundTag("Stored");
                            @Nullable Block containedBlock = Block.getBlockFromName(storedNbt.getString("id"));
                            int containedMeta = storedNbt.getInteger("meta");
                            //lookup the fluid block
                            final @Nullable Block fluid = getFluidByOldName(blockID);
                            /* We found the block, mark it as needing to be fluidlogged later */
                            final boolean needFluidStateAddition = containedBlock != null;
                            /* Attempt to at least keep the fluid, if not the block */
                            if(!needFluidStateAddition) {
                                System.err.println("warning: can't find block " + storedNbt.getString("id") + "at " + pos + ", trying to only place fluid in world");
                                if(fluid != null) {
                                    containedBlock = fluid;
                                    containedMeta = 0;
                                }
                            }
                            //change the block in the chunk
                            if(containedBlock != null) {
                                // Calculate the new block ID, and block ID extension from the block
                                final int blockStateID = Block.BLOCK_STATE_IDS.get(
                                        containedBlock.getStateFromMeta(containedMeta));
                                // Update the block ID and metadata in the original chunk
                                blockIDs[blockIndex] = (byte)(blockStateID >> 4 & 255);
                                metadataArray.set(x, y, z, containedMeta);
                                // Update the extended block ID
                                extIDs.set(x, y, z, blockStateID >> 12 & 15);
                            }
                            //add FluidState to pending
                            if(needFluidStateAddition && fluid != null) {
                                final NBTTagCompound nbt = new NBTTagCompound();
                                nbt.setString("id", String.valueOf(fluid.getRegistryName()));
                                nbt.setLong("pos", pos.toLong());
                                /* Store a fluid location for later */
                                cap.appendTag(nbt);
                            }
                        }
                        // Update the block IDs and metadata in the section
                        section.setByteArray("Blocks", blockIDs);
                        section.setByteArray("Data", metadataArray.getData());
                        section.setByteArray("Add", extIDs.getData());
                        legacyTiles.remove(sectionY);
                        /* We are done. The chunk has been migrated to 1.7. */
                        if(legacyTiles.isEmpty()) return compound;
                    }
                }
            }
        }

        return compound;
    }
}
