/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
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
