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

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

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
    @SuppressWarnings("ConstantConditions")
    @Nullable
    static IFluidStateCapability get(@Nullable ICapabilityProvider p) {
        return p != null && p.hasCapability(Impl.CAPABILITY, null) ? p.getCapability(Impl.CAPABILITY, null) : null;
    }

    //default implementation
    class Impl implements IFluidStateCapability, ICapabilitySerializable<NBTBase>
    {
        @CapabilityInject(IFluidStateCapability.class)
        public static final Capability<IFluidStateCapability> CAPABILITY = null;

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

        //=======================
        //ICapabilitySerializable
        //=======================

        @SuppressWarnings("ConstantConditions")
        @Override
        public boolean hasCapability(@Nonnull Capability<?> capability, @Nullable EnumFacing facing) {
            return capability.equals(CAPABILITY);
        }

        @SuppressWarnings("ConstantConditions")
        @Nullable
        @Override
        public <T> T getCapability(@Nonnull Capability<T> capability, @Nullable EnumFacing facing) {
            return hasCapability(capability, facing) ? CAPABILITY.cast(this) : null;
        }

        @SuppressWarnings("ConstantConditions")
        @Nullable
        @Override
        public NBTBase serializeNBT() { return CAPABILITY.writeNBT(this, null); }

        @SuppressWarnings("ConstantConditions")
        @Override
        public void deserializeNBT(@Nullable NBTBase nbt) { CAPABILITY.readNBT(this, null, nbt); }
    }

    enum Storage implements Capability.IStorage<IFluidStateCapability>
    {
        INSTANCE;

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
                        instance.setFluidState(
                                BlockPos.fromLong(nbt.getLong("pos")),
                                FluidState.of(FluidloggedUtils.getFluidFromBlock(
                                        Optional.ofNullable(Block.getBlockFromName(nbt.getString("id")))
                                                .orElse(Blocks.AIR)
                                        )
                                )
                        );
                    }
                }
            }
        }
    }
}
