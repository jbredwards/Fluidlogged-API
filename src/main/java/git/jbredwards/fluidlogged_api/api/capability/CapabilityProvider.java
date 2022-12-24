package git.jbredwards.fluidlogged_api.api.capability;

import com.google.common.base.Preconditions;
import net.minecraft.nbt.NBTBase;
import net.minecraft.util.EnumFacing;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ICapabilitySerializable;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * A general purpose capability provider class.
 * @author jbred
 *
 */
public class CapabilityProvider<T> implements ICapabilitySerializable<NBTBase>
{
    @Nonnull public final Capability<T> capability;
    @Nonnull public final T instance;
    @Nullable public final EnumFacing face;

    public CapabilityProvider(@Nullable Capability<T> capabilityIn) {
        this(capabilityIn, null);
    }

    public CapabilityProvider(@Nullable Capability<T> capabilityIn, @Nullable T instanceIn) {
        this(capabilityIn, instanceIn, null);
    }

    public CapabilityProvider(@Nullable Capability<T> capabilityIn, @Nullable T instanceIn, @Nullable EnumFacing faceIn) {
        capability = Preconditions.checkNotNull(capabilityIn, "Capability is null, this should never happen!");
        instance = instanceIn != null ? instanceIn : Preconditions.checkNotNull(capability.getDefaultInstance(),
                "Default instance of capability \"%s\" returned null: ", capability.getName());

        face = faceIn;
    }

    @Override
    public boolean hasCapability(@Nullable Capability<?> capabilityIn, @Nullable EnumFacing faceIn) {
        return capabilityIn == capability && face == faceIn;
    }

    @Nullable
    @Override
    public <t> t getCapability(@Nullable Capability<t> capabilityIn, @Nullable EnumFacing faceIn) {
        if(!hasCapability(capabilityIn, faceIn)) return null;
        else return capability.cast(instance);
    }

    @Nonnull
    @Override
    public NBTBase serializeNBT() {
        return Preconditions.checkNotNull(capability.writeNBT(instance, face),
                "NBT for capability \"%s\" returned null, please fix it: ", capability.getName());
    }

    @Override
    public void deserializeNBT(@Nullable NBTBase nbt) { capability.readNBT(instance, face, nbt); }
}
