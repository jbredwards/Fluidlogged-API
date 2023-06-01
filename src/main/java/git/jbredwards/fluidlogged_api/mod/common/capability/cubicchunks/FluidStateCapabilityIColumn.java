package git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks;

import git.jbredwards.fluidlogged_api.api.capability.CapabilityProvider;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateContainer;
import git.jbredwards.fluidlogged_api.api.network.FluidloggedAPINetworkHandler;
import git.jbredwards.fluidlogged_api.mod.common.capability.FluidStateCapabilityVanilla;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageSyncFluidStates;
import io.github.opencubicchunks.cubicchunks.api.world.IColumn;
import io.github.opencubicchunks.cubicchunks.api.world.ICubicWorld;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.event.world.ChunkWatchEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

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
    public FluidStateCapabilityIColumn(@Nonnull IColumn column) { this.column = column; }

    @SubscribeEvent
    static void attach(@Nonnull AttachCapabilitiesEvent<Chunk> event) {
        if(event.getObject() instanceof IColumn && isCubicWorld(event.getObject().getWorld())) {
            event.addCapability(CAPABILITY_ID, new CapabilityProvider<>(CAPABILITY, new FluidStateCapabilityIColumn((IColumn)event.getObject())));
        }

        else {
            final Chunk chunk = event.getObject();
            event.addCapability(CAPABILITY_ID, new CapabilityProvider<>(CAPABILITY, new FluidStateCapabilityVanilla(chunk.x, chunk.z)));
        }
    }

    @SubscribeEvent
    static void sync(@Nonnull ChunkWatchEvent.Watch event) {
        final @Nullable Chunk chunk = event.getChunkInstance();
        if(chunk != null && !isCubicWorld(chunk.getWorld())) {
            final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(chunk);
            if(cap != null) FluidloggedAPINetworkHandler.INSTANCE.sendTo(new MessageSyncFluidStates(chunk, cap), event.getPlayer());
        }
    }

    static boolean isCubicWorld(@Nonnull World world) {
        return world instanceof ICubicWorld && ((ICubicWorld)world).isCubicWorld();
    }

    @Nonnull
    @Override
    public IFluidStateContainer getContainer(@Nonnull BlockPos pos) {
        return getContainer(pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4);
    }

    @Nonnull
    @Override
    public IFluidStateContainer getContainer(int chunkX, int chunkY, int chunkZ) {
        return Objects.requireNonNull(IFluidStateCapability.get(column.getCube(chunkY)))
                .getContainer(chunkX, chunkY, chunkZ);
    }

    //==================================
    //DATA IS STORED IN ICUBE CAPABILITY
    //==================================

    @Nonnull
    @Override
    public NBTBase serializeNBT() { return new NBTTagCompound(); }

    @Override
    public void deserializeNBT(@Nonnull NBTBase nbt) { }
}
