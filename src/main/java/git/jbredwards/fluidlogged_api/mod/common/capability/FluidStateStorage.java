package git.jbredwards.fluidlogged_api.mod.common.capability;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumFacing;
import net.minecraftforge.common.capabilities.Capability;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public enum FluidStateStorage implements Capability.IStorage<IFluidStateCapability>
{
    INSTANCE;

    @Nonnull
    @Override
    public NBTBase writeNBT(@Nullable Capability<IFluidStateCapability> capability, @Nonnull IFluidStateCapability instance, @Nullable EnumFacing side) {
        final NBTTagCompound nbt = new NBTTagCompound();
        nbt.setTag("data", instance.serializeNBT());
        //version int will be changed if the data format changes (it probably won't but lets be safe)
        nbt.setInteger("version", 1);
        return nbt;
    }

    @Override
    public void readNBT(@Nullable Capability<IFluidStateCapability> capability, @Nonnull IFluidStateCapability instance, @Nullable EnumFacing side, @Nonnull NBTBase nbtIn) {
        instance.deserializeNBT(nbtIn);
    }
}
