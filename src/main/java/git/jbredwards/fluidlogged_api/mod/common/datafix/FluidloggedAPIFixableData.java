/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.common.datafix;

import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.datafix.IFluidloggedDataMapper;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.datafix.FixTypes;
import net.minecraft.util.datafix.IFixableData;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.registries.GameData;

import javax.annotation.Nonnull;
import java.util.function.UnaryOperator;

/**
 *
 * @author jbred
 *
 */
public final class FluidloggedAPIFixableData
{
    /**
     * The current data version.
     */
    public static final int LEGACY_DATA_VERSION = 0;
    public static int getToFluidStateDataVersion() { return IFluidloggedDataMapper.MAPPERS.size(); }

    /**
     * Easy way to register chunk data fixers.
     */
    public static void register(@Nonnull final String id, final int version, @Nonnull final UnaryOperator<NBTTagCompound> fixer) {
        FMLCommonHandler.instance().getDataFixer().init(GameData.checkPrefix(id).toString(), version).registerFix(FixTypes.CHUNK,
            new IFixableData() {
                @Override
                public int getFixVersion() { return version; }

                @Nonnull
                @Override
                public NBTTagCompound fixTagCompound(@Nonnull final NBTTagCompound compound) { return fixer.apply(compound); }
            }
        );
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
            @Nonnull final NBTTagCompound capNBT = forgeCaps.getCompoundTag(capID);
            capNBT.setTag("data", cap = capNBT.getTagList("data", Constants.NBT.TAG_COMPOUND));
        }
        else forgeCaps.setTag(capID, cap = forgeCaps.getTagList(capID, Constants.NBT.TAG_COMPOUND));

        // return generated
        return cap;
    }
}
