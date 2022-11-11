package git.jbredwards.fluidlogged_api.mod.common.capability;

import com.google.common.collect.Lists;
import git.jbredwards.fluidlogged_api.api.capability.CapabilityProvider;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.network.FluidloggedAPINetworkHandler;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageSyncFluidStates;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;
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
import java.util.List;
import java.util.function.BiConsumer;

/**
 *
 * @author jbred
 *
 */
public class FluidStateCapabilityNormal implements IFluidStateCapability, IFluidStateContainer
{
    @Nonnull protected final List<FluidState> keys = Lists.newArrayList(FluidState.EMPTY);
    @Nonnull protected final IntSet indexedPositions = new IntOpenHashSet();
    protected int[] data = new int[65535];

    protected final int chunkX, chunkZ;
    public FluidStateCapabilityNormal(int chunkXIn, int chunkZIn) {
        chunkX = chunkXIn;
        chunkZ = chunkZIn;
    }

    @SubscribeEvent
    static void attach(@Nonnull AttachCapabilitiesEvent<Chunk> event) {
        final Chunk chunk = event.getObject();
        event.addCapability(IFluidStateCapability.CAPABILITY_ID, new CapabilityProvider<>(
            IFluidStateCapability.CAPABILITY, new FluidStateCapabilityNormal(chunk.x, chunk.z)
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
    public void forEach(@Nonnull BiConsumer<Integer, FluidState> action) {
        indexedPositions.forEach(indexedPos -> action.accept(indexedPos, keys.get(data[indexedPos])));
    }

    @Override
    public boolean hasFluidState(int serializedPos) {
        return data.length > serializedPos && serializedPos >= 0 && data[serializedPos] != 0;
    }

    @Override
    public int serializePos(@Nonnull BlockPos pos) {
        return pos.getY() << 8 | (pos.getZ() & 15) << 4 | (pos.getX() & 15);
    }

    @Nonnull
    @Override
    public BlockPos deserializePos(int serializedPos) {
        final int x = (chunkX << 4) + (serializedPos & 15);
        final int y = serializedPos >> 8;
        final int z = (chunkZ << 4) + ((serializedPos >> 4) & 15);
        return new BlockPos(x, y, z);
    }

    @Override
    public void clearFluidStates() {
        data = new int[data.length];
        indexedPositions.clear();
        keys.clear();
        keys.add(FluidState.EMPTY);
    }

    @Override
    public void setFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fluidState) {
        setFluidState(serializePos(pos), fluidState);
    }

    @Override
    public void setFluidState(int serializedPos, @Nonnull FluidState fluidState) {
        if(fluidState.isEmpty()) {
            indexedPositions.remove(serializedPos);
            data[serializedPos] = 0;
        }

        else {
            int indexedState = keys.indexOf(fluidState);
            if(indexedState == -1) {
                indexedState = keys.size();
                keys.add(fluidState);
            }

            indexedPositions.add(serializedPos);
            data[serializedPos] = indexedState;
        }
    }

    @Nonnull
    @Override
    public FluidState getFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fallback) {
        return getFluidState(serializePos(pos), fallback);
    }

    @Nonnull
    @Override
    public FluidState getFluidState(int serializedPos, @Nonnull FluidState fallback) {
        return hasFluidState(serializedPos) ? keys.get(data[serializedPos]) : fallback;
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
        //chunks saved after v1.8.2
        if(nbtIn instanceof NBTTagCompound) {
            final NBTTagCompound nbt = (NBTTagCompound)nbtIn;
            final int version = nbt.getInteger("version");
            //v1.8.2
            if(version == 1) {
                nbt.getTagList("data", NBT.TAG_COMPOUND).forEach(tagIn -> {
                    if(tagIn instanceof NBTTagCompound) {
                        NBTTagCompound tag = (NBTTagCompound)tagIn;
                        if(tag.hasKey("id", NBT.TAG_STRING) && tag.hasKey("pos", NBT.TAG_INT)) {
                            FluidState state = FluidState.of(Block.getBlockFromName(tag.getString("id")));
                            if(!state.isEmpty()) setFluidState(tag.getInteger("pos"), state);
                        }
                    }
                });
            }
            //only thrown if the user downgrades fluidlogged api to a version unable to read possible new data
            else throw new IllegalStateException("Could not read chunk data, please update Fluidlogged API to the latest version!");
        }
        //compatibility with pre v1.8.2 chunks
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
