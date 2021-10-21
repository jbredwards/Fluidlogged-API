package git.jbredwards.fluidlogged_api.common.event;

import git.jbredwards.fluidlogged_api.Constants;
import git.jbredwards.fluidlogged_api.common.capability.IFluidStateCapability;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mod.EventBusSubscriber(modid = Constants.MODID)
public final class EventHandler
{
    @SubscribeEvent
    public static void attachCapabilities(AttachCapabilitiesEvent<Chunk> event) {
        event.addCapability(new ResourceLocation(Constants.MODID, "fluid_states"), new IFluidStateCapability.Impl());
    }


}
