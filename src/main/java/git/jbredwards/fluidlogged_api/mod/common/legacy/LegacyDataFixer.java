package git.jbredwards.fluidlogged_api.mod.common.legacy;

import git.jbredwards.fluidlogged_api.mod.Constants;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.datafix.FixTypes;
import net.minecraft.util.datafix.IFixableData;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.NibbleArray;
import net.minecraftforge.common.util.Constants.NBT;
import net.minecraftforge.common.util.ModFixs;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

/* Inspired by https://github.com/Choonster-Minecraft-Mods/TestMod3/blob/5872bd0f917516ce1820d8749bfb75fcc83b0a60/src/main/java/choonster/testmod3/datafix/BlockFlattening.java */
/**
 * A Minecraft data fixer that remaps fluidlogged_api:[fluid]logged_te blocks to use the new save format
 * introduced in v1.7.
 *
 * This is accomplished through two components:
 * <ul>
 *   <li>The data fixer ({@link #fixTagCompound}), which replaces the fluidlogged_te block with its stored block by reading the NBT data, and stores the original fluid in an IFluidStateCapability.</li>
 * </ul>
 */
@Mod.EventBusSubscriber(modid = Constants.MODID)
public final class LegacyDataFixer implements IFixableData {
    /**
     * A map which stores the block ID of the fluidlogged tile entity mapped to the corresponding fluid name.
     *
     * This is reset whenever the integrated server restarts, to make sure that IDs don't cross world boundaries.
     */
    private static Int2ObjectMap<String> FLUID_MAPPINGS = null;

    /**
     * A mapping between the fluid name used by the tile entity and a Forge fluid block.
     */
    private static final Int2ObjectMap<Block> FLUID_ID_MAPPINGS = new Int2ObjectOpenHashMap<>();

    /**
     * The current data version. This is an arbritary value since the mod never used data fixers before.
     */
    public static final int DATA_VERSION = 101;

    /**
     * Creates and registers the data fixers for Fluidlogged API.
     */
    public static void create() {
        final ModFixs modFixs = FMLCommonHandler.instance().getDataFixer().init(Constants.MODID, DATA_VERSION);
        modFixs.registerFix(FixTypes.CHUNK, new LegacyDataFixer());
    }

    /**
     * Called when FMLServerAboutToStartEvent is invoked.
     */
    public static void init() { FLUID_MAPPINGS = new Int2ObjectOpenHashMap<>(); }

    /**
     * Called when FMLServerStoppedEvent is invoked.
     */
    public static void reset() { FLUID_MAPPINGS = null; }

    /**
     * This event handler is called when the registries are first loaded from level.dat. Any fluidlogged_te blocks
     * have their ID and fluid name stored in the {@link #FLUID_MAPPINGS} map.
     */
    @SubscribeEvent
    public static void onMissingBlockMappings(@Nonnull RegistryEvent.MissingMappings<Block> event) {
        for(RegistryEvent.MissingMappings.Mapping<Block> mapping : event.getAllMappings()) {
            if(mapping.key.getResourcePath().endsWith("logged_te")) {
                final String newId = mapping.key.getResourcePath().replace("logged_te", "");
                FLUID_MAPPINGS.put(mapping.id, newId);
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
            final Int2ObjectMap<Map<BlockPos, NBTTagCompound>> legacyTiles = new Int2ObjectOpenHashMap<>();
            for(Iterator<NBTBase> it = tileEntities.iterator(); it.hasNext();) {
                //check if the te is from a legacy version, if it is: add it to fix list
                final NBTTagCompound nbt = (NBTTagCompound)it.next();
                if(nbt.getString("id").equals(Constants.MODID + ":te")) {
                    final int y = nbt.getInteger("y");
                    final int sectionY = y >> 4 << 4;
                    //get fluidlogged te's from the section
                    @Nullable Map<BlockPos, NBTTagCompound> map = legacyTiles.get(sectionY);
                    if(map == null) {
                        map = new HashMap<>();
                        legacyTiles.put(sectionY, map);
                    }
                    //assigns fluidlogged te's to the section
                    map.put(new BlockPos(nbt.getInteger("x"), y, nbt.getInteger("z")), nbt);
                    //remove legacy tile entities from level data
                    it.remove();
                }
            }
            //fix legacy fluidlogged blocks
            if(!legacyTiles.isEmpty()) {
                //initialise forge capability nbt
                final NBTTagCompound forgeCaps = new NBTTagCompound();
                level.setTag("ForgeCaps", forgeCaps);
                //initialise IFluidStateCapability nbt
                final NBTTagList cap = new NBTTagList();
                forgeCaps.setTag(Constants.MODID + ":fluid_states", cap);

                final NBTTagList sections = level.getTagList("Sections", NBT.TAG_COMPOUND);
                for(int i = 0; i < sections.tagCount(); ++i) {
                    final NBTTagCompound section = sections.getCompoundTagAt(i);
                    final int sectionY = section.getByte("Y") << 4;
                    //find the fluidlogged te's within this section to fix
                    final @Nullable Map<BlockPos, NBTTagCompound> sectionLegacyTiles = legacyTiles.get(sectionY);
                    if(sectionLegacyTiles != null) {
                        final NibbleArray metadataArray = new NibbleArray(section.getByteArray("Data"));
                        final byte[] blockIDs = section.getByteArray("Blocks");
                        /* Use a blank extended ID array if not present */
                        final NibbleArray extIDs = section.hasKey("Add", NBT.TAG_BYTE_ARRAY) ?
                                new NibbleArray(section.getByteArray("Add")) : new NibbleArray();
                        //fix each fluidlogged te within the section
                        for(final Map.Entry<BlockPos, NBTTagCompound> entry : sectionLegacyTiles.entrySet()) {
                            final BlockPos pos = entry.getKey();
                            //calculate section indices
                            final int x = pos.getX() & 15;
                            final int y = pos.getY() - sectionY & 15;
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
                                final byte newBlockID = (byte)(blockStateID >> 4 & 255);
                                final byte extID = (byte)(blockStateID >> 12 & 15);
                                // Update the block ID and metadata in the original chunk
                                blockIDs[blockIndex] = newBlockID;
                                metadataArray.set(x, y, z, containedMeta);
                                // Update the extended block ID
                                extIDs.set(x, y, z, extID);
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
