package git.jbredwards.fluidlogged_api.mod.common.capability;

import git.jbredwards.fluidlogged_api.api.capability.CapabilityProvider;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.network.FluidloggedAPINetworkHandler;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.capability.util.FluidStateLayer;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageSyncFluidStates;
import it.unimi.dsi.fastutil.chars.CharLinkedOpenHashSet;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.event.world.ChunkWatchEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.function.BiConsumer;

/**
 * Holds FluidStates within a 16x256x16 area
 * @author jbred
 *
 */
public class FluidStateCapabilityVanilla implements IFluidStateCapability, IFluidStateContainer
{
    protected final CharLinkedOpenHashSet indexedPositions = new CharLinkedOpenHashSet();
    protected final int chunkX, chunkZ;

    @Nonnull
    public FluidStateLayer[] layers = new FluidStateLayer[0]; //use below functions instead of manipulating this directly
    public FluidStateCapabilityVanilla(int chunkXIn, int chunkZIn) {
        chunkX = chunkXIn;
        chunkZ = chunkZIn;
    }

    @SubscribeEvent
    static void attach(@Nonnull AttachCapabilitiesEvent<Chunk> event) {
        final Chunk chunk = event.getObject();
        event.addCapability(CAPABILITY_ID, new CapabilityProvider<>(CAPABILITY, new FluidStateCapabilityVanilla(chunk.x, chunk.z)));
    }

    @SubscribeEvent
    static void sync(@Nonnull ChunkWatchEvent.Watch event) {
        final @Nullable Chunk chunk = event.getChunkInstance();
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
        if(cap != null) FluidloggedAPINetworkHandler.INSTANCE.sendTo(new MessageSyncFluidStates(chunk, cap), event.getPlayer());
    }

    @Override
    public void forEach(@Nonnull BiConsumer<Character, FluidState> action) {
        for(final char indexedPosition : indexedPositions) action.accept(indexedPosition, layers[(indexedPosition >> 8)].data[indexedPosition & 255]);
    }

    @Override
    public boolean hasFluidState(char serializedPos) {
        final int y = serializedPos >> 8;
        if(layers.length <= y) return false;

        final FluidStateLayer layer = layers[y];
        if(layer == null) return false;

        final int xz = serializedPos & 255;
        return layer.data.length > xz && layer.data[xz] != null;
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
        layers = new FluidStateLayer[0];
        indexedPositions.trim();
    }

    @Override
    public void setFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fluidState) {
        setFluidState(serializePos(pos), fluidState); //call other function for easier CC compat
    }

    @Override
    public void setFluidState(char serializedPos, @Nonnull FluidState fluidState) {
        final boolean isEmpty = fluidState.isEmpty();
        final int y = serializedPos >> 8;

        if(layers.length <= y) {
            if(isEmpty) return; //no change

            //increase number of layers
            final FluidStateLayer[] newLayers = new FluidStateLayer[y + 1];
            System.arraycopy(layers, 0, newLayers, 0, layers.length);
            layers = newLayers;
        }

        FluidStateLayer layer = layers[y];
        if(layer == null) {
            if(isEmpty) return; //no change

            //prepare layer
            layers[y] = layer = new FluidStateLayer();
            layer.data = new FluidState[256];
        }

        //remove the FluidState at the given pos
        if(isEmpty) {
            if(indexedPositions.rem(serializedPos)) {
                if(--layer.tracker != Byte.MIN_VALUE) layer.data[serializedPos & 255] = null;
                else if(indexedPositions.isEmpty()) layers = new FluidStateLayer[0];

                //decrease number of cached layers if possible
                else if(layers.length - 1 == y) {
                    int newMaxY = y;
                    while(newMaxY > 0) {
                        if(layers[newMaxY] != null) break;
                        newMaxY--;
                    }

                    final FluidStateLayer[] newLayers = new FluidStateLayer[newMaxY];
                    System.arraycopy(layers, 0, newLayers, 0, newMaxY);
                    layers = newLayers;
                }

                else layers[y] = null;
            }
        }

        //set the FluidState at the given pos
        else {
            if(indexedPositions.add(serializedPos)) layer.tracker++;
            layer.data[serializedPos & 255] = fluidState;
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
        return hasFluidState(serializedPos) ? layers[serializedPos >> 8].data[serializedPos & 255] : fallback;
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
                nbt.getTagList("data", Constants.NBT.TAG_COMPOUND).forEach(tagIn -> {
                    if(tagIn instanceof NBTTagCompound) {
                        NBTTagCompound tag = (NBTTagCompound)tagIn;
                        if(tag.hasKey("id", Constants.NBT.TAG_STRING) && tag.hasKey("pos", Constants.NBT.TAG_INT)) {
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
                    if(nbt.hasKey("id", Constants.NBT.TAG_STRING) && nbt.hasKey("pos", Constants.NBT.TAG_LONG)) {
                        FluidState state = FluidState.of(Block.getBlockFromName(nbt.getString("id")));
                        if(!state.isEmpty()) setFluidState(BlockPos.fromLong(nbt.getLong("pos")), state);
                    }
                }
            }
        }
    }
}
