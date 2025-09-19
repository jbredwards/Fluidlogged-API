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

package git.jbredwards.fluidlogged_api.mod.asm.iface;

import net.minecraft.block.state.IBlockState;
import net.minecraftforge.common.property.IExtendedBlockState;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;

/**
 *
 * @author jbred
 *
 */
public interface IConfigFluidBox
{
    @Nonnull
    static IConfigFluidBox get(@Nonnull final IBlockState state) {
        return (IConfigFluidBox)(state instanceof IExtendedBlockState ? ((IExtendedBlockState)state).getClean() : state);
    }

    @Nullable
    List<HeightBox> getBoxes();
    void setBoxes(@Nullable final List<HeightBox> boxes);
    final class HeightBox
    {
        public final double min, max;
        public HeightBox(final double minIn, final double maxIn) {
            min = minIn;
            max = maxIn;
        }
    }
}
