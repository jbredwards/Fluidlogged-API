package git.jbredwards.fluidlogged_api.common.network;

import git.jbredwards.fluidlogged_api.Constants;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;
import net.minecraftforge.fml.relauncher.Side;

/**
 *
 * @author jbred
 *
 */
public enum NetworkHandler
{
    ;

    public static SimpleNetworkWrapper WRAPPER;

    public static void init() {
        WRAPPER = NetworkRegistry.INSTANCE.newSimpleChannel(Constants.MODID);
        WRAPPER.registerMessage(FluidStateMessage.Handler.INSTANCE, FluidStateMessage.class, 1, Side.CLIENT);
    }
}
