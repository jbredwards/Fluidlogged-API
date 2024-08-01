/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.capability.FluidStateCapabilityVanilla;
import git.jbredwards.fluidlogged_api.mod.common.capability.util.FluidStateLayer;
import it.unimi.dsi.fastutil.chars.AbstractChar2ObjectMap;
import it.unimi.dsi.fastutil.chars.Char2ObjectMap;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.apache.commons.lang3.mutable.MutableInt;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * syncs all FluidStates in a chunk
 * @author jbred
 *
 */
public final class SMessageSyncFluidStates extends AbstractMessage
{
    public Char2ObjectMap.Entry<FluidState>[] data;
    public int chunkX, indexY, chunkZ, layersLength = -1, lowestYPos = Character.MAX_VALUE;

    public SMessageSyncFluidStates() { data = new Char2ObjectMap.Entry[0]; }
    public SMessageSyncFluidStates(@Nonnull final Chunk chunk, @Nonnull final IFluidStateCapability cap) {
        this(chunk.x, 0, chunk.z, cap);
    }

    public SMessageSyncFluidStates(final int chunkXIn, final int indexYIn, final int chunkZIn, @Nonnull final IFluidStateCapability cap) {
        @Nonnull final IFluidStateContainer container = cap.getContainer(indexY);
        @Nonnull final MutableInt currIndex = new MutableInt(-1);

        data = new Char2ObjectMap.Entry[container.getSerializedPositions().size()];
        container.forEach((pos, fluidState) -> data[currIndex.incrementAndGet()] = new AbstractChar2ObjectMap.BasicEntry<>(pos, fluidState));

        isValid = true;
        chunkX = chunkXIn;
        indexY = indexYIn;
        chunkZ = chunkZIn;

        if(container instanceof FluidStateCapabilityVanilla) {
            layersLength = ((FluidStateCapabilityVanilla)container).layers.length;
            lowestYPos = ((FluidStateCapabilityVanilla)container).lowestYPos;
        }
    }

    @Override
    public void read(@Nonnull final PacketBuffer buf) {
        // read pos
        chunkX = buf.readVarInt();
        indexY = buf.readVarInt();
        chunkZ = buf.readVarInt();

        // read data
        data = new Char2ObjectMap.Entry[buf.readVarInt()];
        for(int i = 0; i < data.length; i++) data[i] = new AbstractChar2ObjectMap.BasicEntry<>(buf.readChar(), FluidState.deserialize(buf.readVarInt()));

        // read optimization data
        if(buf.readBoolean()) {
            layersLength = buf.readVarInt();
            lowestYPos = buf.readChar();
        }
    }

    @Override
    public void write(@Nonnull final PacketBuffer buf) {
        // write pos
        buf.writeVarInt(chunkX).writeVarInt(indexY).writeVarInt(chunkZ);

        // write data
        buf.writeVarInt(data.length);
        for(@Nonnull final Char2ObjectMap.Entry<FluidState> entry : data) {
            buf.writeChar(entry.getCharKey());
            buf.writeVarInt(entry.getValue().serialize());
        }

        // write optimization data
        if(layersLength == -1) buf.writeBoolean(false);
        else {
            buf.writeBoolean(true);
            buf.writeVarInt(layersLength).writeChar(lowestYPos);
        }
    }

    public enum Handler implements IClientMessageHandler<SMessageSyncFluidStates>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull final SMessageSyncFluidStates message, @Nonnull final MessageContext ctx) {
            @Nonnull final World world = IClientMessageHandler.getWorldFromContext(ctx);
            @Nonnull final Chunk chunk = world.getChunk(message.chunkX, message.chunkZ);
            @Nullable final IFluidStateCapability cap = IFluidStateCapability.get(chunk);

            if(cap != null) {
                @Nonnull final IFluidStateContainer container = cap.getContainer(message.indexY);
                @Nonnull final char[] oldIndexes = container.getSerializedPositions().toCharArray();
                container.clearFluidStates(); // clear any old fluid states

                // increase array size only once if possible
                if(container instanceof FluidStateCapabilityVanilla) {
                    ((FluidStateCapabilityVanilla)container).layers = new FluidStateLayer[message.layersLength];
                    ((FluidStateCapabilityVanilla)container).lowestYPos = (char)message.lowestYPos;
                }

                // add any new fluid states
                for(@Nonnull final Char2ObjectMap.Entry<FluidState> entry : message.data) {
                    // send changes to client
                    container.setFluidState(entry.getCharKey(), entry.getValue());
                    // re-render block
                    @Nonnull final BlockPos pos = container.deserializePos(entry.getCharKey());
                    FluidloggedUtils.relightFluidBlock(world, pos, chunk, entry.getValue());
                    world.markBlockRangeForRenderUpdate(pos, pos);
                }

                // update removed light levels & renders
                for(final char serializedPos : oldIndexes) {
                    // make sure the cleared pos wasn't replaced prior to re-render
                    if(!container.hasFluidState(serializedPos)) {
                        // re-render block
                        @Nonnull final BlockPos pos = container.deserializePos(serializedPos);
                        FluidloggedUtils.relightFluidBlock(world, pos, chunk, FluidState.EMPTY);
                        world.markBlockRangeForRenderUpdate(pos, pos);
                    }
                }
            }
        }
    }
}
