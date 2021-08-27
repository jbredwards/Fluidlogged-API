package git.jbredwards.fluidlogged_api.common.capability;

import net.minecraft.nbt.NBTBase;
import net.minecraft.util.EnumFacing;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ICapabilitySerializable;

import javax.annotation.Nullable;
import javax.annotation.ParametersAreNonnullByDefault;

/**
 *
 * @author jbred
 *
 */
@ParametersAreNonnullByDefault
public class FluidloggedCapabilityProvider implements ICapabilitySerializable<NBTBase>
{
    public final IFluidloggedCapability instance;

    public FluidloggedCapabilityProvider(IFluidloggedCapability instanceIn) { instance = instanceIn; }

    @Override
    public boolean hasCapability(Capability<?> capability, @Nullable EnumFacing facing) {
        return instance.getCapability() == capability;
    }

    @Nullable
    @Override
    public <T> T getCapability(Capability<T> capability, @Nullable EnumFacing facing) {
        return hasCapability(capability, facing) ? instance.getCapability().cast(instance) : null;
    }

    @Nullable
    @Override
    public NBTBase serializeNBT() { return instance.getCapability().writeNBT(instance, null); }

    @Override
    public void deserializeNBT(NBTBase nbt) { instance.getCapability().readNBT(instance, null, nbt); }
}
