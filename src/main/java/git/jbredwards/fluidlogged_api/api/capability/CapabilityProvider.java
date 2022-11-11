package git.jbredwards.fluidlogged_api.api.capability;

import net.minecraft.nbt.NBTBase;
import net.minecraft.util.EnumFacing;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ICapabilitySerializable;

import javax.annotation.Nullable;

/**
 * A general purpose capability provider class.
 * @author jbred
 *
 */
public class CapabilityProvider<T> implements ICapabilitySerializable<NBTBase>
{
    @Nullable public final Capability<T> capability;
    @Nullable public final T instance;
    @Nullable public final EnumFacing face;

    public CapabilityProvider(@Nullable Capability<T> capability) {
        this(capability, capability != null ? capability.getDefaultInstance() : null);
    }

    public CapabilityProvider(@Nullable Capability<T> capability, @Nullable T instance) {
        this(capability, instance, null);
    }

    public CapabilityProvider(@Nullable Capability<T> capability, @Nullable T instance, @Nullable EnumFacing face) {
        this.capability = capability;
        this.instance = instance;
        this.face = face;
    }

    @Override
    public boolean hasCapability(@Nullable Capability<?> capability, @Nullable EnumFacing facing) {
        return capability == this.capability;
    }

    @Nullable
    @Override
    public <t> t getCapability(@Nullable Capability<t> capability, @Nullable EnumFacing facing) {
        if(!hasCapability(capability, facing)) return null;
        else return this.capability != null ? this.capability.cast(instance) : null;
    }

    @Nullable
    @Override
    public NBTBase serializeNBT() { return capability != null ? capability.writeNBT(instance, face) : null; }

    @Override
    public void deserializeNBT(final NBTBase nbt) { if(capability != null) capability.readNBT(instance, face, nbt); }
}
