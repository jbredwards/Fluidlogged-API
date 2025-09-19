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

package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fml.common.network.ByteBufUtils;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;

/**
 * play fluid vaporize effects for the client from the server
 * @author jbred
 *
 */
public final class SMessageVaporizeEffects extends AbstractMessage
{
    public FluidStack fluid;
    public BlockPos pos;

    public SMessageVaporizeEffects() {}
    public SMessageVaporizeEffects(@Nonnull FluidStack fluidIn, @Nonnull BlockPos posIn) {
        fluid = fluidIn;
        pos = posIn;
        isValid = true;
    }

    @Override
    public void read(@Nonnull PacketBuffer buf) {
        fluid = FluidStack.loadFluidStackFromNBT(ByteBufUtils.readTag(buf));
        pos = buf.readBlockPos();
    }

    @Override
    public void write(@Nonnull PacketBuffer buf) {
        buf.writeCompoundTag(fluid.writeToNBT(new NBTTagCompound()));
        buf.writeBlockPos(pos);
    }

    public enum Handler implements IClientMessageHandler<SMessageVaporizeEffects>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull final SMessageVaporizeEffects message, @Nonnull final MessageContext ctx) {
            message.fluid.getFluid().vaporize(null, IClientMessageHandler.getWorldFromContext(ctx), message.pos, message.fluid);
        }
    }
}
