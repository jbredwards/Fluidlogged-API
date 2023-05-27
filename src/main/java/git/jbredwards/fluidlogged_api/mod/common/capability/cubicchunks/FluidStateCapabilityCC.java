package git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks;

import com.google.common.collect.Lists;
import git.jbredwards.fluidlogged_api.api.capability.CapabilityProvider;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.network.FluidloggedAPINetworkHandler;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageSyncFluidStates;
import it.unimi.dsi.fastutil.chars.CharArrayList;
import it.unimi.dsi.fastutil.chars.CharList;
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
public class FluidStateCapabilityCC implements IFluidStateCapability, IFluidStateContainer
{
    @Nonnull protected final List<FluidState> keys = Lists.newArrayList(FluidState.EMPTY);
    @Nonnull protected final CharList indexedPositions = new CharArrayList();
    protected char[] data = new char[65536];

    protected final int chunkX, chunkZ;
    public FluidStateCapabilityCC(int chunkXIn, int chunkZIn) {
        chunkX = chunkXIn;
        chunkZ = chunkZIn;
    }

    @SubscribeEvent
    static void attach(@Nonnull AttachCapabilitiesEvent<Chunk> event) {
        final Chunk chunk = event.getObject();
        event.addCapability(IFluidStateCapability.CAPABILITY_ID, new CapabilityProvider<>(
            IFluidStateCapability.CAPABILITY, new FluidStateCapabilityCC(chunk.x, chunk.z)
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
        indexedPositions.forEach(indexedPos -> action.accept(indexedPos, keys.get(data[indexedPos])));
    }

    @Override
    public boolean hasFluidState(char serializedPos) {
        return data.length > serializedPos && data[serializedPos] != 0;
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
        data = new char[data.length];
        indexedPositions.clear();
        keys.clear();
        keys.add(FluidState.EMPTY);
    }

    @Override
    public void setFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fluidState) {
        setFluidState(serializePos(pos), fluidState);
    }

    @Override
    public void setFluidState(char serializedPos, @Nonnull FluidState fluidState) {
        if(fluidState.isEmpty()) {
            indexedPositions.rem(serializedPos);
            data[serializedPos] = 0;
        }

        else {
            int indexedState = keys.indexOf(fluidState);
            if(indexedState == -1) {
                indexedState = keys.size();
                keys.add(fluidState);
            }

            if(!indexedPositions.contains(serializedPos))
                indexedPositions.add(serializedPos);

            data[serializedPos] = (char)indexedState;
        }
    }

    @Nonnull
    @Override
    public FluidState getFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fallback) {
        return getFluidState(serializePos(pos), fallback);
    }

    @Nonnull
    @Override
    public FluidState getFluidState(char serializedPos, @Nonnull FluidState fallback) {
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
