package git.jbredwards.fluidlogged_api.mod.common.message;

import git.jbredwards.fluidlogged_api.api.network.IClientMessageHandler;
import git.jbredwards.fluidlogged_api.api.network.message.AbstractMessage;
import net.minecraft.client.Minecraft;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;

/**
 * play fluid vaporize effects for the client from the server
 * @author jbred
 *
 */
public final class MessageVaporizeEffects extends AbstractMessage
{
    public Fluid fluid;
    public BlockPos pos;

    public MessageVaporizeEffects() {}
    public MessageVaporizeEffects(@Nonnull Fluid fluidIn, @Nonnull BlockPos posIn) {
        fluid = fluidIn;
        pos = posIn;
        isValid = true;
    }

    @Override
    public void read(@Nonnull PacketBuffer buf) {
        fluid = FluidRegistry.getFluid(buf.readString(32767));
        pos = buf.readBlockPos();
    }

    @Override
    public void write(@Nonnull PacketBuffer buf) {
        buf.writeString(FluidRegistry.getFluidName(fluid));
        buf.writeBlockPos(pos);
    }

    public enum Handler implements IClientMessageHandler<MessageVaporizeEffects>
    {
        INSTANCE;

        @SideOnly(Side.CLIENT)
        @Override
        public void handleMessage(@Nonnull MessageVaporizeEffects message) {
            message.fluid.vaporize(null, Minecraft.getMinecraft().world, message.pos,
                    new FluidStack(message.fluid, Fluid.BUCKET_VOLUME));
        }
    }
}
