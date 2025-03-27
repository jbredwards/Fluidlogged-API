/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.datafix;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.datafix.IFixableData;
import net.minecraftforge.common.util.Constants;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class FluidloggedAPIFixableData implements IFixableData
{
    /**
     * The current data version.
     */
    @Override
    public int getFixVersion() { return DATA_VERSION; }
    public static final int DATA_VERSION = 103;

    @Nonnull
    @Override
    public NBTTagCompound fixTagCompound(@Nonnull final NBTTagCompound compound) {
        return ToFluidloggedDataFixer.fix(LegacyDataFixer.fix(compound));
    }

    @Nonnull
    public static NBTTagList getOrCreateFluidCapabilityData(@Nonnull final NBTTagCompound level) {
        // initialize or get forge capability nbt
        @Nonnull final NBTTagCompound forgeCaps;
        if(level.hasKey("ForgeCaps", Constants.NBT.TAG_COMPOUND)) forgeCaps = level.getCompoundTag("ForgeCaps");
        else level.setTag("ForgeCaps", forgeCaps = new NBTTagCompound());

        // initialize or get IFluidStateCapability nbt
        @Nonnull final NBTTagList cap;
        @Nonnull final String capID = IFluidStateCapability.CAPABILITY_ID.toString();
        if(forgeCaps.hasKey(capID, Constants.NBT.TAG_COMPOUND)) { // respect modern data if present
            final NBTTagCompound capNBT = forgeCaps.getCompoundTag(capID);
            capNBT.setTag("data", cap = capNBT.getTagList("data", Constants.NBT.TAG_COMPOUND));
        }
        else forgeCaps.setTag(capID, cap = forgeCaps.getTagList(capID, Constants.NBT.TAG_COMPOUND));

        // return generated
        return cap;
    }
}
