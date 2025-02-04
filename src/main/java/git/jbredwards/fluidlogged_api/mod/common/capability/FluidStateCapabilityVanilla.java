/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.capability;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.common.capability.util.FluidStateLayer;
import it.unimi.dsi.fastutil.chars.CharIterator;
import it.unimi.dsi.fastutil.chars.CharLinkedOpenHashSet;
import it.unimi.dsi.fastutil.chars.CharSet;
import it.unimi.dsi.fastutil.chars.CharSets;
import net.minecraft.block.Block;
import net.minecraft.nbt.*;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.common.util.Constants;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Holds FluidStates within a 16x256x16 area
 * @author jbred, Rongmario
 *
 */
public class FluidStateCapabilityVanilla implements IFluidStateCapability, IFluidStateContainer
{
    @Nonnull
    protected final CharLinkedOpenHashSet indexedPositions = new CharLinkedOpenHashSet();
    protected final int offsetX, offsetZ;

    @Nonnull
    public FluidStateLayer[] layers = new FluidStateLayer[0]; // use below functions instead of manipulating this directly
    public char lowestYPos = Character.MAX_VALUE;

    public FluidStateCapabilityVanilla(final int chunkXIn, final int chunkZIn) {
        offsetX = chunkXIn << 4;
        offsetZ = chunkZIn << 4;
    }

    @Override
    public void forEach(@Nonnull final ContainerAction action) {
        for(@Nonnull final CharIterator it = indexedPositions.iterator(); it.hasNext();) {
            final char pos = it.nextChar();
            action.accept(pos, getFluidState(pos));
        }
    }

    @Override
    public boolean hasFluidState(final char serializedPos) {
        final int y = serializedPos >> 8;
        if(y < lowestYPos) return false;

        final int ly = y - lowestYPos;
        return layers.length > ly && layers[ly] != null && layers[ly].data[serializedPos & 255] != null;
    }

    @Override
    public void clearFluidStates() {
        layers = new FluidStateLayer[0];
        lowestYPos = Character.MAX_VALUE;

        if(!indexedPositions.isEmpty()) indexedPositions.clear();
        indexedPositions.trim();
    }

    @Override
    public boolean setFluidState(final char serializedPos, @Nonnull final FluidState fluidState) {
        final boolean isEmpty = fluidState == FluidState.EMPTY;
        final int y = serializedPos >> 8;

        if(y < lowestYPos) {
            if(isEmpty) return false; // no change
            final int dy = lowestYPos - y;
            lowestYPos = (char)y;

            if(layers.length == 0) layers = new FluidStateLayer[1];
            else { // shift layers up to make room for new fluid state below
                @Nonnull final FluidStateLayer[] newLayers = new FluidStateLayer[layers.length + dy + 1];
                System.arraycopy(layers, 0, newLayers, dy, layers.length);
                layers = newLayers;
            }
        }

        final int ly = y - lowestYPos;
        if(layers.length <= ly) {
            if(isEmpty) return false; // no change

            // increase number of layers to make room for new fluid state above
            @Nonnull final FluidStateLayer[] newLayers = new FluidStateLayer[ly + 1];
            System.arraycopy(layers, 0, newLayers, 0, layers.length);
            layers = newLayers;
        }

        @Nullable FluidStateLayer layer = layers[ly];
        if(layer == null) {
            if(isEmpty) return false; // no change
            layers[ly] = layer = new FluidStateLayer();
        }

        final int xz = serializedPos & 255;
        // remove the FluidState at the given pos
        if(isEmpty) {
            if(layer.data[xz] != null && indexedPositions.rem(serializedPos)) {
                if(--layer.tracker != Byte.MIN_VALUE) layer.data[xz] = null; // normal op
                else if(indexedPositions.isEmpty()) clearFluidStates(); // container is now empty, reset it
                else { // remove any unused layers
                    layers[ly] = null;
                    // removed lowest FluidState, remove null layers above and update lowestYPos
                    if(ly == 0) {
                        for(int newMinY = 1; newMinY < layers.length; newMinY++) {
                            if(layers[newMinY] != null) {
                                lowestYPos += newMinY;
                                @Nonnull final FluidStateLayer[] newLayers = new FluidStateLayer[layers.length - newMinY];
                                System.arraycopy(layers, newMinY, newLayers, 0, newLayers.length);
                                layers = newLayers;
                                break;
                            }
                        }
                    }
                    // removed top FluidState, remove null layers below
                    else if(layers.length - 1 == ly) {
                        for(int newMaxY = ly - 1; newMaxY >= 0; newMaxY--) {
                            if(layers[newMaxY] != null) {
                                @Nonnull final FluidStateLayer[] newLayers = new FluidStateLayer[newMaxY + 1];
                                System.arraycopy(layers, 0, newLayers, 0, newLayers.length);
                                layers = newLayers;
                                break;
                            }
                        }
                    }
                }

                return true;
            }
        }

        // set the FluidState at the given pos
        else if(layer.data[xz] != fluidState) {
            if(indexedPositions.add(serializedPos)) layer.tracker++;
            layer.data[xz] = fluidState;
            return true;
        }

        // no change
        return false;
    }

    @Nonnull
    @Override
    public FluidState getFluidState(final char serializedPos, @Nonnull final FluidState fallback) {
        return hasFluidState(serializedPos) ? getFluidState(serializedPos) : fallback;
    }

    @Nonnull
    protected FluidState getFluidState(final char serializedPos) {
        return layers[(serializedPos >> 8) - lowestYPos].data[serializedPos & 255];
    }

    @Nonnull
    @Override
    public IFluidStateContainer getContainer(final int y) { return this; }

    @Nonnull
    @Override
    public CharSet getSerializedPositions() { return CharSets.unmodifiable(indexedPositions); }

    // ===================================
    // NBT SERIALIZATION / DESERIALIZATION
    // ===================================

    @Nonnull
    @Override
    public NBTBase serializeNBT() {
        @Nonnull final NBTTagList data = new NBTTagList();
        forEach((pos, fluidState) -> {
            final NBTTagCompound nbt = new NBTTagCompound();
            nbt.setInteger("pos", pos);
            nbt.setString("id", String.valueOf(fluidState.getBlock().getRegistryName()));

            if(fluidState.getMetadata() != 0) nbt.setInteger("meta", fluidState.getMetadata());
            data.appendTag(nbt);
        });

        @Nonnull final NBTTagCompound nbt = new NBTTagCompound();
        nbt.setTag("data", data);

        // here to prevent layers array from being resized repeatedly when deserializing nbt
        nbt.setInteger("layersLength", layers.length);
        nbt.setInteger("lowestYPos", lowestYPos);

        // version int will be changed if the data format changes
        nbt.setInteger("version", 2);
        return nbt;
    }

    @Override
    public void deserializeNBT(@Nonnull final NBTBase nbtIn) {
        // chunks saved after v1.8.x
        if(nbtIn instanceof NBTTagCompound) {
            @Nonnull final NBTTagCompound nbt = (NBTTagCompound)nbtIn;
            switch(nbt.getInteger("version")) {
                case 1: // compatibility with v2.x.x chunks
                case 2: { // most recent save format
                    if(layers.length == 0 && nbt.hasKey("layersLength", Constants.NBT.TAG_ANY_NUMERIC)) layers = new FluidStateLayer[nbt.getInteger("layersLength")];
                    if(lowestYPos == Character.MAX_VALUE && nbt.hasKey("lowestYPos", Constants.NBT.TAG_ANY_NUMERIC)) lowestYPos = (char)nbt.getInteger("lowestYPos");
                    nbt.getTagList("data", Constants.NBT.TAG_COMPOUND).forEach(tagIn -> {
                        @Nonnull final NBTTagCompound tag = (NBTTagCompound)tagIn;
                        if(tag.hasKey("id", Constants.NBT.TAG_STRING) && tag.hasKey("pos", Constants.NBT.TAG_ANY_NUMERIC)) {
                            @Nullable final Block block = Block.getBlockFromName(tag.getString("id"));
                            if(block == null) return;

                            @Nonnull final NBTPrimitive posNbt = (NBTPrimitive)tag.getTag("pos");
                            final char pos = posNbt instanceof NBTTagLong ? serializePos(BlockPos.fromLong(posNbt.getLong())) : (char)posNbt.getInt();

                            //noinspection deprecation
                            setFluidState(pos, FluidState.of(block.getStateFromMeta(tag.getInteger("meta"))));
                        }
                    });

                    return;
                }

                // only thrown if the user downgrades fluidlogged api to a version unable to read possible new data
                default: throw new IllegalArgumentException("Could not read chunk data, please update Fluidlogged API to the latest version!");
            }
        }

        // compatibility with v1.8.x chunks
        else if(nbtIn instanceof NBTTagList) ((NBTTagList)nbtIn).forEach(tagIn -> {
            if(tagIn instanceof NBTTagCompound) {
                @Nonnull final NBTTagCompound nbt = (NBTTagCompound)tagIn;
                if(nbt.hasKey("id", Constants.NBT.TAG_STRING) && nbt.hasKey("pos", Constants.NBT.TAG_LONG)) {
                    setFluidState(BlockPos.fromLong(nbt.getLong("pos")), FluidState.of(Block.getBlockFromName(nbt.getString("id"))));
                }
            }
        });
    }

    // ========================================
    // POSITION SERIALIZATION / DESERIALIZATION
    // ========================================

    @Override
    public int serializeX(final int x) { return x & 15; }

    @Override
    public int serializeY(final int y) { return y << 8; }

    @Override
    public int serializeZ(final int z) { return (z & 15) << 4; }

    @Override
    public int deserializeX(final char serializedPos) { return offsetX | (serializedPos & 15); }

    @Override
    public int deserializeY(final char serializedPos) { return serializedPos >> 8; }

    @Override
    public int deserializeZ(final char serializedPos) { return offsetZ | ((serializedPos >> 4) & 15); }
}
