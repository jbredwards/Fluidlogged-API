package git.jbredwards.fluidlogged_api.api.capability;

import net.minecraft.nbt.NBTBase;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.INBTSerializable;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Hold IFluidStateContainers
 * @author jbred
 *
 */
public interface IFluidStateCapability extends INBTSerializable<NBTBase>
{
    @SuppressWarnings("ConstantConditions")
    @CapabilityInject(IFluidStateCapability.class)
    @Nonnull Capability<IFluidStateCapability> CAPABILITY = null;
    @Nonnull ResourceLocation CAPABILITY_ID = new ResourceLocation("fluidlogged_api", "fluid_states");

    //having exactly ONE container per 16x16x16 area of the world is expected
    @Nonnull IFluidStateContainer getContainer(@Nonnull BlockPos pos);
    @Nonnull IFluidStateContainer getContainer(int chunkX, int chunkY, int chunkZ);

    //get this from a capability provider
    @SuppressWarnings("ConstantConditions")
    @Nullable
    static IFluidStateCapability get(@Nullable ICapabilityProvider p) {
        return p != null && p.hasCapability(CAPABILITY, null) ? p.getCapability(CAPABILITY, null) : null;
    }
}
