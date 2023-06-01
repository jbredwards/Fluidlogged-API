package git.jbredwards.fluidlogged_api.mod.common.capability.cubicchunks;

import git.jbredwards.fluidlogged_api.api.capability.CapabilityProvider;
import git.jbredwards.fluidlogged_api.api.capability.IFluidStateCapability;
import git.jbredwards.fluidlogged_api.api.network.FluidloggedAPINetworkHandler;
import git.jbredwards.fluidlogged_api.mod.common.capability.FluidStateCapabilityVanilla;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageSyncFluidStates;
import io.github.opencubicchunks.cubicchunks.api.world.CubeWatchEvent;
import io.github.opencubicchunks.cubicchunks.api.world.ICube;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Cubic Chunks mod compat, holds FluidStates within a 16x16x16 area
 * @author jbred
 *
 */
public class FluidStateCapabilityICube extends FluidStateCapabilityVanilla
{
    protected final int chunkY;
    public FluidStateCapabilityICube(int chunkXIn, int chunkYIn, int chunkZIn) {
        super(chunkXIn, chunkZIn);
        chunkY = chunkYIn;
    }

    @SubscribeEvent
    static void attachToCube(@Nonnull AttachCapabilitiesEvent<ICube> event) {
        final ICube cube = event.getObject();
        event.addCapability(CAPABILITY_ID, new CapabilityProvider<>(CAPABILITY, new FluidStateCapabilityICube(cube.getX(), cube.getY(), cube.getZ())));
    }

    @SubscribeEvent
    static void sync(@Nonnull CubeWatchEvent event) {
        final ICube cube = event.getCube();
        final @Nullable IFluidStateCapability cap = IFluidStateCapability.get(cube);
        if(cap != null) FluidloggedAPINetworkHandler.INSTANCE.sendTo(new MessageSyncFluidStates(cube.getX(), cube.getY(), cube.getZ(), cap), event.getPlayer());
    }

    @Override
    public char serializePos(@Nonnull BlockPos pos) {
        return (char)((pos.getY() & 15) << 8 | (pos.getZ() & 15) << 4 | (pos.getX() & 15));
    }

    @Nonnull
    @Override
    public BlockPos deserializePos(char serializedPos) {
        final int x = (chunkX << 4) | (serializedPos & 15);
        final int y = (chunkY << 4) | ((serializedPos >> 8) & 15);
        final int z = (chunkZ << 4) | ((serializedPos >> 4) & 15);
        return new BlockPos(x, y, z);
    }
}
