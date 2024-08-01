/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.iface;

import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public interface IWaterHeight
{
    @Nullable
    IConfigFluidBox.HeightBox getBox();
    void setBox(@Nullable final IConfigFluidBox.HeightBox waterHeight);
}
