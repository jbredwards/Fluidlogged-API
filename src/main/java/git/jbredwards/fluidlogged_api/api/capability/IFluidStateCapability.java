/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.api.capability;

import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IHardcodedCapability;
import net.minecraft.nbt.NBTBase;
import net.minecraft.util.ResourceLocation;
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
@SuppressWarnings("ConstantConditions")
public interface IFluidStateCapability extends INBTSerializable<NBTBase>
{
    @CapabilityInject(IFluidStateCapability.class)
    @Nonnull Capability<IFluidStateCapability> CAPABILITY = null;
    @Nonnull ResourceLocation CAPABILITY_ID = new ResourceLocation(FluidloggedAPI.MODID, "fluid_states");

    //get this from a capability provider
    @Nullable
    static IFluidStateCapability get(@Nullable final ICapabilityProvider provider) {
        if(provider instanceof IHardcodedCapability) return ((IHardcodedCapability)provider).getFluidStateCapability();
        return provider != null && provider.hasCapability(CAPABILITY, null) ? provider.getCapability(CAPABILITY, null) : null;
    }

    //having exactly ONE container per 16x16x16 area of the world is expected
    @Nonnull
    IFluidStateContainer getContainer(final int y);
}
