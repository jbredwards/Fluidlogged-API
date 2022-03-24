package git.jbredwards.fluidlogged_api.mod.common.legacy;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.capability.IFluidStateCapability;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.ObjectIntIdentityMap;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.datafix.FixTypes;
import net.minecraft.util.datafix.IFixableData;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.chunk.NibbleArray;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.common.util.ModFixs;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.event.world.ChunkDataEvent;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.registries.GameData;
import org.apache.commons.lang3.tuple.Pair;

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
@Mod.EventBusSubscriber
public class LegacyDataFixer implements IFixableData {
    /**
     * A map which stores the block ID of the fluidlogged tile entity mapped to the corresponding fluid name.
     *
     * This is reset whenever the integrated server restarts, to make sure that IDs don't cross world boundaries.
     */
    private static Int2ObjectMap<String> FLUID_MAPPINGS = null;

    /**
     * A mapping between the fluid name used by the tile entity and a Forge fluid.
     */
    private static final HashMap<String, Fluid> FLUID_NAME_MAPPINGS = new HashMap<>();

    /**
     * The current data version. This is an arbritary value since the mod never used data fixers before.
     */
    public static final int DATA_VERSION = 101;

    /**
     * Creates and registers the data fixers for Fluidlogged API.
     */
    public static void create() {
        final ModFixs modFixs = FMLCommonHandler.instance().getDataFixer().init(git.jbredwards.fluidlogged_api.mod.Constants.MODID, DATA_VERSION);
        modFixs.registerFix(FixTypes.CHUNK, new LegacyDataFixer());
    }

    /**
     * Called when FMLServerAboutToStartEvent is invoked.
     */
    public static void onServerAboutToStart() {
        FLUID_MAPPINGS = new Int2ObjectOpenHashMap<>();
    }

    /**
     * Called when FMLServerStoppedEvent is invoked.
     */
    public static void onServerStopped() {
        FLUID_MAPPINGS = null;
    }

    /**
     * This event handler is called when the registries are first loaded from level.dat. Any fluidlogged_te blocks
     * have their ID and fluid name stored in the {@link #FLUID_MAPPINGS} map.
     */
    @SubscribeEvent
    public static void onMissingMapping(RegistryEvent.MissingMappings<Block> event) {
        for(RegistryEvent.MissingMappings.Mapping<Block> mapping : event.getMappings()) {
            if(mapping.key.getResourcePath().endsWith("logged_te")) {
                FLUID_MAPPINGS.put(mapping.id, mapping.key.getResourcePath().replace("logged_te", ""));
                /* Prevent a warning */
                mapping.ignore();
            }
        }
    }

    @Override
    public int getFixVersion() { return DAVA_VERSION; }

    /**
     * Try to find a Forge fluid given an old TE fluid name.
     */
    @Nullable
    private static Fluid getFluidByOldName(String fluidName) {
        return FLUID_NAME_MAPPINGS.computeIfAbsent(fluidName, name -> {
            for(Fluid entry : FluidRegistry.getRegisteredFluids().values()) {
                if(new ResourceLocation(entry.getName()).getResourcePath().equals(name)) {
                    return entry;
                }
            }
            System.out.println("Can't find fluid: " + name);
            return null;
        });
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
    public static void onChunkDataLoad(ChunkDataEvent.Load event) {
        if(!event.getWorld().isRemote && event.getData().hasKey("Fluidlogged_Fluids")) {
            /* Get the list of fluid locations created below */
            NBTTagList theFluids = event.getData().getTagList("Fluidlogged_Fluids", Constants.NBT.TAG_COMPOUND);
            /* We deal with the capability directly to avoid relighting while a chunk is being loaded */
            final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(event.getChunk());
            if(cap == null)
                throw new IllegalStateException("Expecting fluid state capability to be available");
            /* Loop through each location and add it to the capability */
            for(NBTBase fluidNbtBase : theFluids) {
                NBTTagCompound fluidNbt = (NBTTagCompound)fluidNbtBase;
                int x = fluidNbt.getInteger("x");
                int y = fluidNbt.getInteger("y");
                int z = fluidNbt.getInteger("z");
                String fluidName = fluidNbt.getString("fluid");
                Fluid fluid = getFluidByOldName(fluidName);
                if(fluid != null) {
                    cap.setFluidState(new BlockPos(x, y, z), FluidState.of(fluid));
                }
            }
            /* We are done. The chunk has been migrated to 1.7. */
        }
    }

    /**
     * Rewrites fluidlogged_te blocks to have the block ID and metadata of their contents.
     */
    @Override
    @Nonnull
    public NBTTagCompound fixTagCompound(@Nonnull NBTTagCompound compound) {
        // Skip datafix if it isn't needed
        if(!FLUID_MAPPINGS.isEmpty()) {
            /* Retrieve the blockstate ID map. This is needed to find the right values to put into the NBT later. */
            final ObjectIntIdentityMap<IBlockState> blockStateIDMap = GameData.getBlockStateIDMap();
            try
            {
                NBTTagCompound chunkCompound = compound.getCompoundTag("Level");
                NBTTagList fluids = null;
                int chunkX = chunkCompound.getInteger("xPos");
                int chunkZ = chunkCompound.getInteger("zPos");
                final ChunkPos chunkPos = new ChunkPos(chunkX, chunkZ);
                NBTTagList tileEntities = chunkCompound.getTagList("TileEntities", 10);
                NBTTagList sections = chunkCompound.getTagList("Sections", 10);

                // Maps TileEntity positions to pairs of TileEntity compound tags and their index in the `TileEntities` list tag
                final Map<BlockPos, Pair<Integer, NBTTagCompound>> tileEntityMap = new HashMap<>();

                for (int tileEntityIndex = 0; tileEntityIndex < tileEntities.tagCount(); tileEntityIndex++) {
                    final NBTTagCompound tileEntityNBT = tileEntities.getCompoundTagAt(tileEntityIndex);
                    if (!tileEntityNBT.hasNoTags()) {
                        final BlockPos pos = new BlockPos(tileEntityNBT.getInteger("x"), tileEntityNBT.getInteger("y"), tileEntityNBT.getInteger("z"));
                        tileEntityMap.put(pos, Pair.of(tileEntityIndex, tileEntityNBT));
                    }
                }

                for (int l = 0; l < sections.tagCount(); ++l)
                {
                    NBTTagCompound section = sections.getCompoundTagAt(l);
                    int sectionY = section.getByte("Y");
                    byte[] blockIDs = section.getByteArray("Blocks");
                    final NibbleArray metadataArray = new NibbleArray(section.getByteArray("Data"));
                    /* Some chunks (most, in modded) need extended IDs, but not all do */
                    boolean isExtended = section.hasKey("Add", Constants.NBT.TAG_BYTE_ARRAY);
                    /* Use a blank extended ID array if not */
                    final NibbleArray extIDs = isExtended ? new NibbleArray(section.getByteArray("Add")) : new NibbleArray();
                    for (int blockIndex = 0; blockIndex < blockIDs.length; ++blockIndex)
                    {
                        final int x = blockIndex & 15;
                        final int y = blockIndex >> 8 & 15;
                        final int z = blockIndex >> 4 & 15;
                        final int blockIDExtension = extIDs.get(x, y, z);
                        /* Find the real block ID by combining the extended ID and the normal ID */
                        final int blockID = blockIDExtension << 8 | (blockIDs[blockIndex] & 255);
                        if (FLUID_MAPPINGS.containsKey(blockID)) {
                            /* This used to be a fluidlogged tile entity */
                            BlockPos blockPos = chunkPos.getBlock(x, (sectionY << 4) + y, z);
                            /* Try to find the tile entity data at this position */
                            final Pair<Integer, NBTTagCompound> tileEntityData = tileEntityMap.get(blockPos);
                            if(tileEntityData != null) {
                                NBTTagCompound te = tileEntityData.getValue();
                                if(te.hasKey("Stored", Constants.NBT.TAG_COMPOUND)) {
                                    final NBTTagCompound nbt = te.getCompoundTag("Stored");
                                    /* Look up the originally stored block and meta */
                                    Block containedBlock = Block.getBlockFromName(nbt.getString("id"));
                                    int containedMeta = nbt.getInteger("meta");
                                    boolean needFluidStateAddition = false;
                                    if(containedBlock == null) {
                                        /* Attempt to at least keep the fluid, if not the block */
                                        System.out.println("warning: can't find block " + nbt.getString("id") + "at " + blockPos + ", trying to only place fluid in world");
                                        Fluid fluid = getFluidByOldName(FLUID_MAPPINGS.get(blockID));
                                        if(fluid != null) {
                                            containedBlock = fluid.getBlock();
                                            containedMeta = 0;
                                        }
                                    } else {
                                        /* We found the block, mark it as needing to be fluidlogged later */
                                        needFluidStateAddition = true;
                                    }
                                    if(containedBlock != null) {
                                        /* Find the block state */
                                        IBlockState newBlockState = containedBlock.getStateFromMeta(containedMeta);
                                        // Calculate the new block ID, block ID extension and metadata from the block state's ID
                                        final int blockStateID = blockStateIDMap.get(newBlockState);
                                        final byte newBlockID = (byte) (blockStateID >> 4 & 255);
                                        final byte newBlockIDExtension = (byte) (blockStateID >> 12 & 15);
                                        final byte newMetadata = (byte) (blockStateID & 15);
                                        // Update the block ID and metadata in the original chunk
                                        blockIDs[blockIndex] = newBlockID;
                                        metadataArray.set(x, y, z, newMetadata);

                                        // Update the extended block ID if we need it
                                        if (newBlockIDExtension != 0) {
                                            /* Set the flag to make sure we save the Add array later */
                                            isExtended = true;
                                            extIDs.set(x, y, z, newBlockIDExtension);
                                        }
                                    }
                                    if(needFluidStateAddition) {
                                        /*
                                         * Ideally, we would just use setFluidState right here.
                                         * Unfortunately, data fixers run early, before the data
                                         * is converted to an actual Chunk and proper APIs are
                                         * available. Therefore, we store some extra data in the
                                         * NBT that we can retrieve later once Forge has built a
                                         * proper Chunk for us.
                                         */
                                        if(fluids == null) {
                                            fluids = new NBTTagList();
                                        }
                                        NBTTagCompound fluidNbt = new NBTTagCompound();
                                        fluidNbt.setInteger("x", blockPos.getX());
                                        fluidNbt.setInteger("y", blockPos.getY());
                                        fluidNbt.setInteger("z", blockPos.getZ());
                                        fluidNbt.setString("fluid", FLUID_MAPPINGS.get(blockID));
                                        /* Store a fluid location later */
                                        fluids.appendTag(fluidNbt);
                                    }
                                }
                            }
                        }
                    }
                    // Update the block ID and metadata in the section
                    section.setByteArray("Blocks", blockIDs);
                    section.setByteArray("Data", metadataArray.getData());

                    // Update the block ID extensions in the section, if present
                    if (isExtended) {
                        section.setByteArray("Add", extIDs.getData());
                    }
                }
                if(fluids != null)
                    compound.setTag("Fluidlogged_Fluids", fluids);
            }
            catch (Exception e)
            {
                System.err.println("Unable to datafix legacy fluidlogged blocks.");
                e.printStackTrace();
            }
        }
        
        return compound;
    }
}
