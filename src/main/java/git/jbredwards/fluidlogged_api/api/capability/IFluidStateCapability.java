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
 * Holds {@link IFluidStateContainer IFluidStateContainers}. All methods in this interface should only be used internally! If you're a modder,
 * see {@link git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils FluidloggedUtils}.
 *
 * @since 1.9.0
 * @author jbred
 *
 */
@SuppressWarnings("ConstantConditions")
public interface IFluidStateCapability extends INBTSerializable<NBTBase>
{
    @CapabilityInject(IFluidStateCapability.class)
    @Nonnull Capability<IFluidStateCapability> CAPABILITY = null;
    @Nonnull ResourceLocation CAPABILITY_ID = new ResourceLocation(FluidloggedAPI.MODID, "fluid_states");

    /**
     * @param provider ICapabilityProvider.
     * @return This from a capability provider.
     *
     * @since 1.9.0
     * @author jbred
     */
    @Nullable
    static IFluidStateCapability get(@Nullable final ICapabilityProvider provider) {
        if(provider instanceof IHardcodedCapability) return ((IHardcodedCapability)provider).getFluidStateCapability();
        return provider != null && provider.hasCapability(CAPABILITY, null) ? provider.getCapability(CAPABILITY, null) : null;
    }

    /**
     * Having at minimum ONE container per 16x16x16 area of the world is expected.
     * @param y Y position (block position format, not chunk position format).
     * @return The container stored at the y position.
     *
     * @since 3.0.0
     * @author jbred
     */
    @Nonnull
    IFluidStateContainer getContainer(final int y);
}
