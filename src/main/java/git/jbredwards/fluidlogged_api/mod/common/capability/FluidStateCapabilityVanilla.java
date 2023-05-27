package git.jbredwards.fluidlogged_api.mod.common.capability;

import git.jbredwards.fluidlogged_api.api.capability.CapabilityProvider;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.network.FluidloggedAPINetworkHandler;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageSyncFluidStates;
import it.unimi.dsi.fastutil.chars.CharOpenHashSet;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants.NBT;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.event.world.ChunkWatchEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.function.BiConsumer;

/**
 *
 * @author jbred
 *
 */
public class FluidStateCapabilityVanilla implements IFluidStateCapability, IFluidStateContainer
{
    // @Nonnull protected final CharArrayList indexedPositions = new CharArrayList();
    protected final CharOpenHashSet indexedPositions = new CharOpenHashSet();
    protected final int chunkX, chunkZ;

    @Nonnull protected byte[] tracker = new byte[0];
    @Nonnull protected FluidState[][] data = new FluidState[0][];

    public FluidStateCapabilityVanilla(int chunkXIn, int chunkZIn) {
        chunkX = chunkXIn;
        chunkZ = chunkZIn;
    }

    @SubscribeEvent
    static void attach(@Nonnull AttachCapabilitiesEvent<Chunk> event) {
        final Chunk chunk = event.getObject();
        event.addCapability(IFluidStateCapability.CAPABILITY_ID, new CapabilityProvider<>(
            IFluidStateCapability.CAPABILITY, new FluidStateCapabilityVanilla(chunk.x, chunk.z)
        ));
    }

    @SubscribeEvent
    static void sync(@Nonnull ChunkWatchEvent.Watch event) {
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(event.getChunkInstance());
        if(cap != null) {
            final IMessage message = new MessageSyncFluidStates(event.getChunkInstance(), cap);
            FluidloggedAPINetworkHandler.INSTANCE.sendTo(message, event.getPlayer());
        }
    }

    @Override
    public void forEach(@Nonnull BiConsumer<Character, FluidState> action) {
        for (char indexedPosition : indexedPositions) {
            action.accept(indexedPosition, data[(indexedPosition >> 8)][((indexedPosition & 15) << 4) | ((indexedPosition >> 4) & 15)]);
        }
    }

    @Override
    public boolean hasFluidState(char serializedPos) {
        int y = serializedPos >> 8;
        if (data.length <= y) {
            return false;
        }
        FluidState[] states = data[y];
        if (states == null) {
            return false;
        }
        return states[((serializedPos & 15) << 4) | ((serializedPos >> 4) & 15)] != null;
    }

    @Override
    public char serializePos(@Nonnull BlockPos pos) {
        return (char)(pos.getY() << 8 | (pos.getZ() & 15) << 4 | (pos.getX() & 15));
    }

    @Nonnull
    @Override
    public BlockPos deserializePos(char serializedPos) {
        final int x = (chunkX << 4) | (serializedPos & 15);
        final int y = serializedPos >> 8;
        final int z = (chunkZ << 4) | ((serializedPos >> 4) & 15);
        return new BlockPos(x, y, z);
    }

    @Override
    public void clearFluidStates() {
        for (int i = 0; i < data.length; i++) {
            data[i] = null;
            tracker[i] = 0;
        }
        indexedPositions.trim();
    }

    @Override
    public void setFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fluidState) {
        int y = pos.getY();
        if (data.length <= y) {
            FluidState[][] newData = new FluidState[y + 1][];
            System.arraycopy(data, 0, newData, 0, data.length);
            data = newData;
            byte[] newTracker = new byte[y + 1];
            System.arraycopy(tracker, 0, newTracker, 0, tracker.length);
            tracker = newTracker;
        }
        FluidState[] states = data[y];
        if (states == null) {
            if (fluidState.isEmpty()) {
                return;
            }
            data[y] = states = new FluidState[256];
            tracker[y] = 0;
        } else if (fluidState.isEmpty()) {
            char serializedPos = serializePos(pos);
            if (indexedPositions.rem(serializedPos)) {
                if (--tracker[y] == 0) {
                    data[y] = null;
                    for (byte b : tracker) {
                        if (b > 0) {
                            return;
                        }
                    }
                    data = new FluidState[0][];
                    tracker = new byte[0];
                } else {
                    states[((serializedPos & 15) << 4) | ((serializedPos >> 4) & 15)] = null;
                }
            }
            return;
        }
        char serializedPos = serializePos(pos);
        if (indexedPositions.add(serializedPos)) {
            tracker[y]++;
        }
        states[((serializedPos & 15) << 4) | ((serializedPos >> 4) & 15)] = fluidState;
    }

    @Override
    public void setFluidState(char serializedPos, @Nonnull FluidState fluidState) {
        int y = serializedPos >> 8;
        if (data.length <= y) {
            FluidState[][] newData = new FluidState[y + 1][];
            System.arraycopy(data, 0, newData, 0, data.length);
            data = newData;
            byte[] newTracker = new byte[y + 1];
            System.arraycopy(tracker, 0, newTracker, 0, tracker.length);
            tracker = newTracker;
        }
        FluidState[] states = data[y];
        if (states == null) {
            if (fluidState.isEmpty()) {
                return;
            }
            data[y] = states = new FluidState[256];
            tracker[y] = 0;
        } else if (fluidState.isEmpty()) {
            if (indexedPositions.rem(serializedPos)) {
                if (--tracker[y] == 0) {
                    data[y] = null;
                    for (byte b : tracker) {
                        if (b > 0) {
                            return;
                        }
                    }
                    data = new FluidState[0][];
                    tracker = new byte[0];
                } else {
                    states[((serializedPos & 15) << 4) | ((serializedPos >> 4) & 15)] = null;
                }
            }
            return;
        }
        if (indexedPositions.add(serializedPos)) {
            tracker[y]++;
        }
        states[((serializedPos & 15) << 4) | ((serializedPos >> 4) & 15)] = fluidState;
    }

    @Nonnull
    @Override
    public FluidState getFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fallback) {
        return getFluidState(serializePos(pos), fallback);
    }

    @Nonnull
    @Override
    public FluidState getFluidState(char serializedPos, @Nonnull FluidState fallback) {
        int y = serializedPos >> 8;
        if (data.length <= y) {
            return FluidState.EMPTY;
        }
        FluidState[] states = data[y];
        if (states == null) {
            return FluidState.EMPTY;
        }
        FluidState fluidState = states[((serializedPos & 15) << 4) | ((serializedPos >> 4) & 15)];
        return fluidState == null ? FluidState.EMPTY : fluidState;
    }

    @Nonnull
    @Override
    public IFluidStateContainer getContainer(@Nonnull BlockPos pos) { return this; }

    @Nonnull
    @Override
    public IFluidStateContainer getContainer(int x, int y, int z) { return this; }

    @Nonnull
    @Override
    public NBTTagList serializeNBT() {
        final NBTTagList data = new NBTTagList();
        forEach((pos, fluidState) -> {
            final NBTTagCompound nbt = new NBTTagCompound();
            nbt.setInteger("pos", pos);
            nbt.setString("id", String.valueOf(fluidState.getBlock().getRegistryName()));
            data.appendTag(nbt);
        });

        return data;
    }

    @Override
    public void deserializeNBT(@Nonnull NBTBase nbtIn) {
        //chunks saved after v1.8.x
        if(nbtIn instanceof NBTTagCompound) {
            final NBTTagCompound nbt = (NBTTagCompound)nbtIn;
            final int version = nbt.getInteger("version");
            //v1.9.0.0
            if(version == 1) {
                nbt.getTagList("data", NBT.TAG_COMPOUND).forEach(tagIn -> {
                    if(tagIn instanceof NBTTagCompound) {
                        NBTTagCompound tag = (NBTTagCompound)tagIn;
                        if(tag.hasKey("id", NBT.TAG_STRING) && tag.hasKey("pos", NBT.TAG_INT)) {
                            FluidState state = FluidState.of(Block.getBlockFromName(tag.getString("id")));
                            if(!state.isEmpty()) setFluidState((char)tag.getInteger("pos"), state);
                        }
                    }
                });
            }
            //only thrown if the user downgrades fluidlogged api to a version unable to read possible new data
            else throw new IllegalStateException("Could not read chunk data, please update Fluidlogged API to the latest version!");
        }

        //compatibility with v1.8.x chunks
        else if(nbtIn instanceof NBTTagList) {
            for(NBTBase tag : (NBTTagList)nbtIn) {
                if(tag instanceof NBTTagCompound) {
                    NBTTagCompound nbt = (NBTTagCompound)tag;
                    if(nbt.hasKey("id", NBT.TAG_STRING) && nbt.hasKey("pos", NBT.TAG_LONG)) {
                        FluidState state = FluidState.of(Block.getBlockFromName(nbt.getString("id")));
                        if(!state.isEmpty()) setFluidState(BlockPos.fromLong(nbt.getLong("pos")), state);
                    }
                }
            }
        }
    }

}
