package git.jbredwards.fluidlogged_api.common.capability;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.capabilities.ICapabilitySerializable;
import net.minecraftforge.common.util.Constants;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

/**
 * allows chunks to store fluid states
 * @author jbred
 *
 */
public interface IFluidStateCapability
{
    @Nonnull Iterable<Map.Entry<BlockPos, FluidState>> getFluidStates();
    //don't call directly, instead use FluidloggedUtils#getFluidState please!
    @Nonnull FluidState getFluidState(@Nonnull BlockPos pos);
    //don't call directly, instead use FluidloggedUtils#setFluidState please!
    void setFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fluid);
    //get this from a capability provider
    @Nullable
    static IFluidStateCapability get(@Nullable ICapabilityProvider p) {
        return p != null && p.hasCapability(Impl.CAPABILITY, null) ? p.getCapability(Impl.CAPABILITY, null) : null;
    }

    //default implementation
    class Impl implements IFluidStateCapability
    {
        @CapabilityInject(IFluidStateCapability.class)
        public static Capability<IFluidStateCapability> CAPABILITY = null;

        @Nonnull
        protected final Map<BlockPos, FluidState> fluidStates = new HashMap<>();

        @Nonnull
        @Override
        public Iterable<Map.Entry<BlockPos, FluidState>> getFluidStates() { return fluidStates.entrySet(); }

        @Nonnull
        @Override
        public FluidState getFluidState(@Nonnull BlockPos pos) {
            final @Nullable FluidState state = fluidStates.get(pos);
            return state == null ? FluidState.EMPTY : state;
        }

        @Override
        public void setFluidState(@Nonnull BlockPos pos, @Nonnull FluidState fluid) {
            if(fluid.isEmpty()) fluidStates.remove(pos);
            else fluidStates.put(pos, fluid);
        }
    }

    final class Provider implements ICapabilitySerializable<NBTBase>
    {
        @Nonnull public final IFluidStateCapability instance = new Impl();

        @Override
        public boolean hasCapability(@Nullable Capability<?> capability, @Nullable EnumFacing facing) {
            return capability == Impl.CAPABILITY;
        }

        @Nullable
        @Override
        public <T> T getCapability(@Nullable Capability<T> capability, @Nullable EnumFacing facing) {
            if(!hasCapability(capability, facing)) return null;
            else return Impl.CAPABILITY.cast(instance);
        }

        @Nullable
        @Override
        public NBTBase serializeNBT() { return Impl.CAPABILITY.writeNBT(instance, null); }

        @Override
        public void deserializeNBT(final NBTBase nbt) { Impl.CAPABILITY.readNBT(instance, null, nbt); }
    }

    enum Storage implements Capability.IStorage<IFluidStateCapability>
    {
        INSTANCE;

        @Nonnull
        @Override
        public NBTBase writeNBT(@Nullable Capability<IFluidStateCapability> capability, @Nonnull IFluidStateCapability instance, @Nullable EnumFacing side) {
            final NBTTagList list = new NBTTagList();
            for(Map.Entry<BlockPos, FluidState> entry : instance.getFluidStates()) {
                NBTTagCompound nbt = new NBTTagCompound();
                nbt.setLong("pos", entry.getKey().toLong());
                nbt.setString("id", Objects.requireNonNull(
                        entry.getValue().getBlock().getRegistryName()).toString());

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
                        if(nbt.hasKey("pos", Constants.NBT.TAG_LONG) && nbt.hasKey("id", Constants.NBT.TAG_STRING)) {
                            BlockPos pos = BlockPos.fromLong(nbt.getLong("pos"));
                            FluidState state = FluidState.of(FluidloggedUtils.getFluidFromBlock(
                                    Optional.ofNullable(Block.getBlockFromName(nbt.getString("id"))).orElse(Blocks.AIR)));

                            instance.setFluidState(pos, state);
                        }
                    }
                }
            }
        }
    }
}
