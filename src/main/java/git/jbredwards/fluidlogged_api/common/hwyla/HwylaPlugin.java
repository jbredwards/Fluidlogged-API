package git.jbredwards.fluidlogged_api.common.hwyla;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import mcp.mobius.waila.api.*;
import net.minecraft.item.ItemStack;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * WIP
 * @author jbred
 *
 */
public final class HwylaPlugin implements IWailaPlugin
{
    @Override
    public void register(IWailaRegistrar registrar) {

    }

    public enum Provider implements IWailaDataProvider
    {
        INSTANCE;

        @Nonnull
        @Override
        public List<String> getWailaHead(ItemStack itemStack, List<String> tooltip, IWailaDataAccessor accessor, IWailaConfigHandler config) {
            final FluidState fluidState = FluidState.get(accessor.getWorld(), accessor.getPosition());
            //default
            return tooltip;
        }
    }
}
