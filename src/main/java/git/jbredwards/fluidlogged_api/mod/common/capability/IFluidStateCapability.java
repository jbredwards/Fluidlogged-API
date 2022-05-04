package git.jbredwards.fluidlogged_api.mod.common.capability;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import it.unimi.dsi.fastutil.longs.Long2ObjectMap;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.EnumFacing;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.capabilities.ICapabilitySerializable;
import net.minecraftforge.common.util.Constants;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;

/**
 * allows chunks to store fluid states
 * @author jbred
 *
 */
public interface IFluidStateCapability
{
    @CapabilityInject(IFluidStateCapability.class)
    Capability<IFluidStateCapability> CAPABILITY = null;

    //these are internal methods only, only use them if you have to
    @Nonnull Long2ObjectMap<FluidState> getFluidStates();
    void setFluidState(long pos, @Nonnull FluidState fluid);

    //get this from a capability provider
    @SuppressWarnings("ConstantConditions")
    @Nullable
    static IFluidStateCapability get(@Nullable ICapabilityProvider p) {
        return p != null && p.hasCapability(CAPABILITY, null) ? p.getCapability(CAPABILITY, null) : null;
    }

    //default implementation
    class Impl implements IFluidStateCapability
    {
        @Nonnull
        protected final Long2ObjectMap<FluidState> fluidStates = new Long2ObjectOpenHashMap<>();

        @Nonnull
        @Override
        public Long2ObjectMap<FluidState> getFluidStates() { return fluidStates; }

        @Override
        public void setFluidState(long pos, @Nonnull FluidState fluidState) {
            if(fluidState.isEmpty()) fluidStates.remove(pos);
            else fluidStates.put(pos, fluidState);
        }
    }

    @SuppressWarnings("ConstantConditions")
    final class Provider implements ICapabilitySerializable<NBTBase>
    {
        final IFluidStateCapability instance = CAPABILITY.getDefaultInstance();

        @Override
        public boolean hasCapability(@Nullable Capability<?> capabilityIn, @Nullable EnumFacing facing) {
            return capabilityIn == CAPABILITY;
        }

        @Nullable
        @Override
        public <T> T getCapability(@Nullable Capability<T> capabilityIn, @Nullable EnumFacing facing) {
            if(!hasCapability(capabilityIn, facing)) return null;
            else return CAPABILITY.cast(instance);
        }

        @Nonnull
        @Override
        public NBTBase serializeNBT() { return CAPABILITY.writeNBT(instance, null); }

        @Override
        public void deserializeNBT(@Nonnull NBTBase nbt) { CAPABILITY.readNBT(instance, null, nbt); }
    }

    enum Storage implements Capability.IStorage<IFluidStateCapability>
    {
        INSTANCE;

        @Nonnull
        @Override
        public NBTBase writeNBT(@Nullable Capability<IFluidStateCapability> capability, @Nonnull IFluidStateCapability instance, @Nullable EnumFacing side) {
            final NBTTagList list = new NBTTagList();
            for(Map.Entry<Long, FluidState> entry : instance.getFluidStates().entrySet()) {
                NBTTagCompound nbt = new NBTTagCompound();
                nbt.setLong("pos", entry.getKey());
                nbt.setString("id", String.valueOf(entry.getValue().getBlock().getRegistryName()));

                list.appendTag(nbt);
            }

            return list;
        }

        @Override
        public void readNBT(@Nullable Capability<IFluidStateCapability> capability, @Nonnull IFluidStateCapability instance, @Nullable EnumFacing side, @Nullable NBTBase nbtIn) {
            if(nbtIn instanceof NBTTagList) {
                for(NBTBase tag : (NBTTagList)nbtIn) {
                    if(tag instanceof NBTTagCompound) {
                        NBTTagCompound nbt = (NBTTagCompound)tag;
                        if(nbt.hasKey("id", Constants.NBT.TAG_STRING) && nbt.hasKey("pos", Constants.NBT.TAG_LONG)) {
                            FluidState state = FluidState.of(Block.getBlockFromName(nbt.getString("id")));
                            if(!state.isEmpty()) instance.setFluidState(nbt.getLong("pos"), state);
                        }
                    }
                }
            }
        }
    }
}
