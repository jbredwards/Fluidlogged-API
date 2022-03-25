package git.jbredwards.fluidlogged_api.mod.common.legacy;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.Constants;
import git.jbredwards.fluidlogged_api.mod.common.capability.IFluidStateCapability;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.datafix.FixTypes;
import net.minecraft.util.datafix.IFixableData;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.chunk.NibbleArray;
import net.minecraftforge.common.util.Constants.NBT;
import net.minecraftforge.common.util.ModFixs;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.event.world.ChunkDataEvent;
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
 *   <li>The data fixer ({@link #fixTagCompound}), which replaces the fluidlogged_te block with its stored block by reading the NBT data, and stores the original fluid in an NBT list.</li>
 *   <li>When the chunk is loaded, an event handler ({@link #onChunkDataLoad}) detects the extra NBT list and populates the fluid states in the IFluidStateCapability.</li>
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
     * Try to perform a function if the resulting fluid block isn't null.
     */
    @Nullable
    private static Block getFluidByOldName(int name) {
        final @Nullable String mapping = FLUID_MAPPINGS.get(name);
        if(mapping != null) {
            final @Nullable Fluid fluid = FluidRegistry.getFluid(mapping);
            if(fluid != null) return fluid.getBlock();
            else System.err.println("Can't find fluid: " + mapping);
        }

        else System.err.println("Can't find fluid from id: " + name);
        return null;
    }

    /**
     * Called *after* the chunk has been "fixed" by {@link #fixTagCompound}.
     *
     * At this point the world is in a valid state to be loaded by Forge, but there are no fluids
     * present where there used to be. We lookup any old fluid locations and save these states into
     * the {@link IFluidStateCapability}.
     *
     * See the body of {@link #fixTagCompound} for an explanation of why this cannot be done in-place
     * within the data fixer itself.
     */
    @SubscribeEvent
    public static void onChunkDataLoad(@Nonnull ChunkDataEvent.Load event) {
        if(!event.getWorld().isRemote && event.getData().hasKey("Fluidlogged_Fluids", NBT.TAG_LIST)) {
            /* Get the list of fluid locations created below */
            final NBTTagList theFluids = event.getData().getTagList("Fluidlogged_Fluids", NBT.TAG_COMPOUND);
            event.getData().removeTag("Fluidlogged_Fluids");
            /* We deal with the capability directly to avoid relighting while a chunk is being loaded */
            final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(event.getChunk());
            if(cap == null) throw new IllegalStateException("Expecting fluid state capability to be available");
            /* Loop through each location and add it to the capability */
            for(int i = 0; i < theFluids.tagCount(); i++) {
                final NBTTagCompound fluidNbt = theFluids.getCompoundTagAt(i);
                final @Nullable Block fluid = getFluidByOldName(fluidNbt.getInteger("fluid"));
                if(fluid != null) cap.setFluidState(BlockPos.fromLong(fluidNbt.getLong("pos")), FluidState.of(fluid));
            }
            /* We are done. The chunk has been migrated to 1.7. */
        }
    }

    /**
     * Rewrites fluidlogged_te blocks to have the block ID and metadata of their contents.
     */
    @Nonnull
    @Override
    public NBTTagCompound fixTagCompound(@Nonnull NBTTagCompound compound) {
        // Skip datafix if it isn't needed
        if(!FLUID_MAPPINGS.isEmpty()) {
            final NBTTagCompound level = compound.getCompoundTag("Level");
            final NBTTagList tileEntities = level.getTagList("TileEntities", NBT.TAG_COMPOUND);
            //saves the pos & nbt of the tiles that need fixing
            final Map<BlockPos, NBTTagCompound> legacyTiles = new HashMap<>();
            for(Iterator<NBTBase> it = tileEntities.iterator(); it.hasNext();) {
                //check if the te is from a legacy version, if it is: add it to fix list
                NBTTagCompound nbt = (NBTTagCompound)it.next();
                if(nbt.getString("id").equals(Constants.MODID + ":te")) {
                    legacyTiles.put(new BlockPos(nbt.getInteger("x"), nbt.getInteger("y"), nbt.getInteger("z")), nbt);
                    //remove legacy tile entities from level data
                    it.remove();
                }
            }
            //fix legacy fluidlogged blocks
            if(!legacyTiles.isEmpty()) {
                final NBTTagList fluids = new NBTTagList();
                final ChunkPos chunkPos = new ChunkPos(level.getInteger("xPos"), level.getInteger("zPos"));
                final NBTTagList sections = level.getTagList("Sections", NBT.TAG_COMPOUND);
                for(int i = 0; i < sections.tagCount(); ++i) {
                    final NBTTagCompound section = sections.getCompoundTagAt(i);
                    final NibbleArray metadataArray = new NibbleArray(section.getByteArray("Data"));
                    final byte[] blockIDs = section.getByteArray("Blocks");
                    final int sectionY = section.getByte("Y");
                    /* Some chunks (most, in modded) need extended IDs, but not all do */
                    boolean isExtended = section.hasKey("Add", NBT.TAG_BYTE_ARRAY);
                    /* Use a blank extended ID array if not */
                    final NibbleArray extIDs = isExtended ? new NibbleArray(section.getByteArray("Add")) : new NibbleArray();
                    for(int blockIndex = 0; blockIndex < blockIDs.length; ++blockIndex) {
                        final int x = blockIndex & 15, y = blockIndex >> 8 & 15, z = blockIndex >> 4 & 15;
                        //get legacy tile if present
                        final BlockPos pos = chunkPos.getBlock(x, (sectionY << 4) + y, z);
                        final @Nullable NBTTagCompound te = legacyTiles.get(pos);
                        //do fix
                        if(te != null) {
                            /* Find the real block ID by combining the extended ID and the normal ID */
                            final int blockID = extIDs.get(x, y, z) << 8 | (blockIDs[blockIndex] & 255);
                            if(FLUID_MAPPINGS.containsKey(blockID)) {
                                final NBTTagCompound storedNbt = te.getCompoundTag("Stored");
                                /* Look up the originally stored block and meta */
                                @Nullable Block containedBlock = Block.getBlockFromName(storedNbt.getString("id"));
                                int containedMeta = storedNbt.getInteger("meta");
                                /* We found the block, mark it as needing to be fluidlogged later */
                                final boolean needFluidStateAddition = containedBlock != null;
                                /* Attempt to at least keep the fluid, if not the block */
                                if(!needFluidStateAddition) {
                                    System.err.println("warning: can't find block " + storedNbt.getString("id") + "at " + pos + ", trying to only place fluid in world");
                                    final @Nullable Block fluid = getFluidByOldName(blockID);
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
                                    /* Set the flag to make sure we save the Add array later */
                                    isExtended = true;
                                    extIDs.set(x, y, z, extID);
                                }
                                //add FluidState to pending
                                if(needFluidStateAddition) {
                                    /*
                                     * Ideally, we would just use setFluidState right here.
                                     * Unfortunately, data fixers run early, before the data
                                     * is converted to an actual Chunk and proper APIs are
                                     * available. Therefore, we store some extra data in the
                                     * NBT that we can retrieve later once Forge has built a
                                     * proper Chunk for us.
                                     */
                                    final NBTTagCompound nbt = new NBTTagCompound();
                                    nbt.setLong("pos", pos.toLong());
                                    nbt.setInteger("fluid", blockID);
                                    /* Store a fluid location for later */
                                    fluids.appendTag(nbt);
                                }
                            }
                            legacyTiles.remove(pos);
                            if(legacyTiles.isEmpty()) {
                                // Update the block IDs and metadata in the section
                                section.setByteArray("Blocks", blockIDs);
                                section.setByteArray("Data", metadataArray.getData());
                                if(isExtended) section.setByteArray("Add", extIDs.getData());
                                /* Store fluid locations for later */
                                compound.setTag("Fluidlogged_Fluids", fluids);
                                //there's no more fluidlogged te's in the chunk
                                return compound;
                            }
                        }
                    }
                }
            }
        }
        
        return compound;
    }
}
