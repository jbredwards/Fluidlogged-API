/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import io.github.opencubicchunks.cubicchunks.api.world.IColumn;
import net.minecraft.block.Block;
import net.minecraft.nbt.*;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 * Cubic Chunks mod compat
 * @author jbred
 *
 */
public class FluidStateCapabilityIColumn implements IFluidStateCapability
{
    @Nonnull
    protected final IColumn column;
    public FluidStateCapabilityIColumn(@Nonnull final Chunk columnIn) { column = (IColumn)columnIn; }

    @Nonnull
    @Override
    public IFluidStateContainer getContainer(final int y) {
        return Objects.requireNonNull(IFluidStateCapability.get(column.getCube(y >> 4))).getContainer(y);
    }

    @Nonnull
    @Override
    public NBTBase serializeNBT() { return new NBTTagByte((byte)0); } // data is stored in ICube capability

    @Override
    public void deserializeNBT(@Nonnull final NBTBase nbtIn) {
        // ==========================
        // convert any old chunk data
        // ==========================

        // chunks saved after v1.8.x
        if(nbtIn instanceof NBTTagCompound) {
            @Nonnull final NBTTagCompound nbt = (NBTTagCompound)nbtIn;
            switch(nbt.getInteger("version")) {
                case 1: // compatibility with v2.x.x chunks
                case 2: { // most recent save format
                    nbt.getTagList("data", Constants.NBT.TAG_COMPOUND).forEach(tagIn -> {
                        @Nonnull final NBTTagCompound tag = (NBTTagCompound)tagIn;
                        if(tag.hasKey("id", Constants.NBT.TAG_STRING) && tag.hasKey("pos", Constants.NBT.TAG_ANY_NUMERIC)) {
                            @Nullable final Block block = Block.getBlockFromName(tag.getString("id"));
                            if(block == null) return;

                            @Nonnull final NBTPrimitive posNbt = (NBTPrimitive)tag.getTag("pos");
                            if(posNbt instanceof NBTTagLong) {
                                @Nonnull final BlockPos pos = BlockPos.fromLong(posNbt.getLong());
                                getContainer(pos.getY()).setFluidState(pos, FluidState.of(block.getStateFromMeta(tag.getInteger("meta"))));
                            }

                            else getContainer((char)posNbt.getInt() >> 8).setFluidState((char)posNbt.getInt(), FluidState.of(block.getStateFromMeta(tag.getInteger("meta"))));
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
                    @Nonnull final BlockPos pos = BlockPos.fromLong(nbt.getLong("pos"));
                    getContainer(pos.getY()).setFluidState(pos, FluidState.of(Block.getBlockFromName(nbt.getString("id"))));
                }
            }
        });
    }
}
